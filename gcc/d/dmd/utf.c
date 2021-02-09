
/* Compiler implementation of the D programming language
 * Copyright (C) 2003-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/utf.c
 */

/// Description of UTF-8 in [1].  Unicode non-characters and private-use
/// code points described in [2],[4].
///
/// References:
/// [1] http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8
/// [2] http://en.wikipedia.org/wiki/Unicode
/// [3] http://unicode.org/faq/utf_bom.html
/// [4] http://www.unicode.org/versions/Unicode6.1.0/ch03.pdf

#include "utf.h"

/* The following encodings are valid, except for the 5 and 6 byte
 * combinations:
 *      0xxxxxxx
 *      110xxxxx 10xxxxxx
 *      1110xxxx 10xxxxxx 10xxxxxx
 *      11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
 *      111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
 *      1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
 */
const unsigned UTF8_STRIDE[256] =
{
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
    4,4,4,4,4,4,4,4,5,5,5,5,6,6,0xFF,0xFF,
};

// UTF-8 decoding errors
char const UTF8_DECODE_OUTSIDE_CODE_SPACE[] = "Outside Unicode code space";
char const UTF8_DECODE_TRUNCATED_SEQUENCE[] = "Truncated UTF-8 sequence";
char const UTF8_DECODE_OVERLONG[]           = "Overlong UTF-8 sequence";
char const UTF8_DECODE_INVALID_TRAILER[]    = "Invalid trailing code unit";
char const UTF8_DECODE_INVALID_CODE_POINT[] = "Invalid code point decoded";

// UTF-16 decoding errors
char const UTF16_DECODE_TRUNCATED_SEQUENCE[]= "Truncated UTF-16 sequence";
char const UTF16_DECODE_INVALID_SURROGATE[] = "Invalid low surrogate";
char const UTF16_DECODE_UNPAIRED_SURROGATE[]= "Unpaired surrogate";
char const UTF16_DECODE_INVALID_CODE_POINT[]= "Invalid code point decoded";

/// The Unicode code space is the range of code points [0x000000,0x10FFFF]
/// except the UTF-16 surrogate pairs in the range [0xD800,0xDFFF]
/// and non-characters (which end in 0xFFFE or 0xFFFF).
bool utf_isValidDchar(dchar_t c)
{
    // TODO: Whether non-char code points should be rejected is pending review
    // largest character code point
    if (c > 0x10FFFF)
        return false;
    // surrogate pairs
    if (0xD800 <= c && c <= 0xDFFF)
        return false;
    // non-characters
    if ((c & 0xFFFFFE) == 0x00FFFE)
        return false;
    return true;
}

/*******************************
 * Return !=0 if unicode alpha.
 * Use table from C99 Appendix D.
 */

bool isUniAlpha(dchar_t c)
{
    size_t high = ALPHA_TABLE_LENGTH - 1;
    // Shortcut search if c is out of range
    size_t low
        = (c < ALPHA_TABLE[0][0] || ALPHA_TABLE[high][1] < c) ? high + 1 : 0;
    // Binary search
    while (low <= high)
    {
        size_t mid = (low + high) >> 1;
        if (c < ALPHA_TABLE[mid][0])
            high = mid - 1;
        else if (ALPHA_TABLE[mid][1] < c)
            low = mid + 1;
        else
        {
            assert(ALPHA_TABLE[mid][0] <= c && c <= ALPHA_TABLE[mid][1]);
            return true;
        }
    }
    return false;
}

/**
 * Returns the code length of c in code units.
 */

int utf_codeLengthChar(dchar_t c)
{
    if (c <= 0x7F)
        return 1;
    if (c <= 0x7FF)
        return 2;
    if (c <= 0xFFFF)
        return 3;
    if (c <= 0x10FFFF)
        return 4;
    assert(false);
    return 6;
}

int utf_codeLengthWchar(dchar_t c)
{
    return c <= 0xFFFF ? 1 : 2;
}

/**
 * Returns the code length of c in code units for the encoding.
 * sz is the encoding: 1 = utf8, 2 = utf16, 4 = utf32.
 */

int utf_codeLength(int sz, dchar_t c)
{
    if (sz == 1)
        return utf_codeLengthChar(c);
    if (sz == 2)
        return utf_codeLengthWchar(c);
    assert(sz == 4);
    return 1;
}

void utf_encodeChar(utf8_t *s, dchar_t c)
{
    assert(s != NULL);
    assert(utf_isValidDchar(c));
    if (c <= 0x7F)
    {
        s[0] = static_cast<utf8_t>(c);
    }
    else if (c <= 0x07FF)
    {
        s[0] = static_cast<utf8_t>(0xC0 | (c >> 6));
        s[1] = static_cast<utf8_t>(0x80 | (c & 0x3F));
    }
    else if (c <= 0xFFFF)
    {
        s[0] = static_cast<utf8_t>(0xE0 | (c >> 12));
        s[1] = static_cast<utf8_t>(0x80 | ((c >> 6) & 0x3F));
        s[2] = static_cast<utf8_t>(0x80 | (c & 0x3F));
    }
    else if (c <= 0x10FFFF)
    {
        s[0] = static_cast<utf8_t>(0xF0 | (c >> 18));
        s[1] = static_cast<utf8_t>(0x80 | ((c >> 12) & 0x3F));
        s[2] = static_cast<utf8_t>(0x80 | ((c >> 6) & 0x3F));
        s[3] = static_cast<utf8_t>(0x80 | (c & 0x3F));
    }
    else
        assert(0);
}

void utf_encodeWchar(utf16_t *s, dchar_t c)
{
    assert(s != NULL);
    assert(utf_isValidDchar(c));
    if (c <= 0xFFFF)
    {
        s[0] = static_cast<utf16_t>(c);
    }
    else
    {
        s[0] = static_cast<utf16_t>((((c - 0x010000) >> 10) & 0x03FF) + 0xD800);
        s[1] = static_cast<utf16_t>(((c - 0x010000) & 0x03FF) + 0xDC00);
    }
}

void utf_encode(int sz, void *s, dchar_t c)
{
    if (sz == 1)
        utf_encodeChar((utf8_t *)s, c);
    else if (sz == 2)
        utf_encodeWchar((utf16_t *)s, c);
    else
    {
        assert(sz == 4);
        *((utf32_t *)s) = c;
    }
}

/********************************************
 * Decode a UTF-8 sequence as a single UTF-32 code point.
 * Returns:
 *      NULL    success
 *      !=NULL  error message string
 */

const char *utf_decodeChar(utf8_t const *s, size_t len, size_t *pidx, dchar_t *presult)
{
    assert(s != NULL);
    assert(pidx != NULL);
    assert(presult != NULL);
    size_t i = (*pidx)++;
    assert(i < len);
    utf8_t u = s[i];
    // Pre-stage results for ASCII and error cases
    *presult = u;

    //printf("utf_decodeChar(s = %02x, %02x, %02x len = %d)\n", u, s[1], s[2], len);

    // Get expected sequence length
    size_t n = UTF8_STRIDE[u];
    switch (n)
    {
    case 1:                             // ASCII
        return UTF8_DECODE_OK;
    case 2: case 3: case 4:             // multi-byte UTF-8
        break;
    default:                            // 5- or 6-byte sequence
        return UTF8_DECODE_OUTSIDE_CODE_SPACE;
    }
    if (len < i + n)                    // source too short
        return UTF8_DECODE_TRUNCATED_SEQUENCE;

    // Pick off 7 - n low bits from first code unit
    utf32_t c = u & ((1 << (7 - n)) - 1);
    /* The following combinations are overlong, and illegal:
     *      1100000x (10xxxxxx)
     *      11100000 100xxxxx (10xxxxxx)
     *      11110000 1000xxxx (10xxxxxx 10xxxxxx)
     *      11111000 10000xxx (10xxxxxx 10xxxxxx 10xxxxxx)
     *      11111100 100000xx (10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx)
     */
    utf8_t u2 = s[++i];
    // overlong combination
    if ((u & 0xFE) == 0xC0 ||
        (u == 0xE0 && (u2 & 0xE0) == 0x80) ||
        (u == 0xF0 && (u2 & 0xF0) == 0x80) ||
        (u == 0xF8 && (u2 & 0xF8) == 0x80) ||
        (u == 0xFC && (u2 & 0xFC) == 0x80))
        return UTF8_DECODE_OVERLONG;
    // Decode remaining bits
    for (n += i - 1; i != n; ++i)
    {
        u = s[i];
        if ((u & 0xC0) != 0x80)         // trailing bytes are 10xxxxxx
            return UTF8_DECODE_INVALID_TRAILER;
        c = (c << 6) | (u & 0x3F);
    }
    if (!utf_isValidDchar(c))
        return UTF8_DECODE_INVALID_CODE_POINT;
    *pidx = i;
    *presult = c;
    return UTF8_DECODE_OK;
}

/********************************************
 * Decode a UTF-16 sequence as a single UTF-32 code point.
 * Returns:
 *      NULL    success
 *      !=NULL  error message string
 */

const char *utf_decodeWchar(utf16_t const *s, size_t len, size_t *pidx, dchar_t *presult)
{
    assert(s != NULL);
    assert(pidx != NULL);
    assert(presult != NULL);
    size_t i = (*pidx)++;
    assert(i < len);
    // Pre-stage results for ASCII and error cases
    utf32_t u = *presult = s[i];

    if (u < 0x80)                       // ASCII
        return UTF16_DECODE_OK;
    if (0xD800 <= u && u <= 0xDBFF)     // Surrogate pair
    {   if (len <= i + 1)
            return UTF16_DECODE_TRUNCATED_SEQUENCE;
        utf16_t u2 = s[i + 1];
        if (u2 < 0xDC00 || 0xDFFF < u)
            return UTF16_DECODE_INVALID_SURROGATE;
        u = ((u - 0xD7C0) << 10) + (u2 - 0xDC00);
        ++*pidx;
    }
    else if (0xDC00 <= u && u <= 0xDFFF)
        return UTF16_DECODE_UNPAIRED_SURROGATE;
    if (!utf_isValidDchar(u))
        return UTF16_DECODE_INVALID_CODE_POINT;
    *presult = u;
    return UTF16_DECODE_OK;
}
