/**
 * Functions related to UTF encoding.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/root/utf.d, _utf.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_utf.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/root/utf.d
 */

module dmd.root.utf;

@nogc nothrow pure @safe:

/// The Unicode code space is the range of code points [0x000000,0x10FFFF]
/// except the UTF-16 surrogate pairs in the range [0xD800,0xDFFF]
bool utf_isValidDchar(dchar c)
{
    // TODO: Whether non-char code points should be rejected is pending review.
    // 0xFFFE and 0xFFFF are valid for internal use, like Phobos std.utf.isValidDChar
    // See also https://issues.dlang.org/show_bug.cgi?id=1357
    if (c < 0xD800) // Almost all characters in a typical document.
        return true;
    if (c > 0xDFFF && c <= 0x10FFFF)
        return true;
    return false;
}

/**
 * Returns the code length of c in code units.
 */
int utf_codeLengthChar(dchar c)
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
}

int utf_codeLengthWchar(dchar c)
{
    return c <= 0xFFFF ? 1 : 2;
}

/**
 * Returns the code length of c in code units for the encoding.
 * sz is the encoding: 1 = utf8, 2 = utf16, 4 = utf32.
 */
int utf_codeLength(int sz, dchar c)
{
    if (sz == 1)
        return utf_codeLengthChar(c);
    if (sz == 2)
        return utf_codeLengthWchar(c);
    assert(sz == 4);
    return 1;
}

void utf_encodeChar(char* s, dchar c) @system
{
    assert(s !is null);
    assert(utf_isValidDchar(c));
    if (c <= 0x7F)
    {
        s[0] = cast(char)c;
    }
    else if (c <= 0x07FF)
    {
        s[0] = cast(char)(0xC0 | (c >> 6));
        s[1] = cast(char)(0x80 | (c & 0x3F));
    }
    else if (c <= 0xFFFF)
    {
        s[0] = cast(char)(0xE0 | (c >> 12));
        s[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        s[2] = cast(char)(0x80 | (c & 0x3F));
    }
    else if (c <= 0x10FFFF)
    {
        s[0] = cast(char)(0xF0 | (c >> 18));
        s[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
        s[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        s[3] = cast(char)(0x80 | (c & 0x3F));
    }
    else
        assert(0);
}

void utf_encodeWchar(wchar* s, dchar c) @system
{
    assert(s !is null);
    assert(utf_isValidDchar(c));
    if (c <= 0xFFFF)
    {
        s[0] = cast(wchar)c;
    }
    else
    {
        s[0] = cast(wchar)((((c - 0x010000) >> 10) & 0x03FF) + 0xD800);
        s[1] = cast(wchar)(((c - 0x010000) & 0x03FF) + 0xDC00);
    }
}

void utf_encode(int sz, void* s, dchar c) @system
{
    if (sz == 1)
        utf_encodeChar(cast(char*)s, c);
    else if (sz == 2)
        utf_encodeWchar(cast(wchar*)s, c);
    else
    {
        assert(sz == 4);
        *(cast(dchar*)s) = c;
    }
}

/********************************************
 * Checks whether an Unicode code point is a bidirectional
 * control character.
 */
bool isBidiControl(dchar c)
{
    // Source: https://www.unicode.org/versions/Unicode15.0.0, table 23-3.
    switch(c)
    {
        case '\u061C':
        case '\u200E':
        case '\u200F':
        case '\u202A': .. case '\u202E':
        case '\u2066': .. case '\u2069':
            return true;
        default:
            return false;
    }
}

/********************************************
 * Decode a UTF-8 sequence as a single UTF-32 code point.
 * Params:
 *      s = UTF-8 sequence
 *      ridx = starting index in s[], updated to reflect number of code units decoded
 *      rresult = set to character decoded
 * Returns:
 *      null on success, otherwise error message string
 */
string utf_decodeChar(const(char)[] s, ref size_t ridx, out dchar rresult)
{
    // UTF-8 decoding errors
    static immutable string UTF8_DECODE_OK = null; // no error
    static immutable string UTF8_DECODE_OUTSIDE_CODE_SPACE = "Outside Unicode code space";
    static immutable string UTF8_DECODE_TRUNCATED_SEQUENCE = "Truncated UTF-8 sequence";
    static immutable string UTF8_DECODE_OVERLONG = "Overlong UTF-8 sequence";
    static immutable string UTF8_DECODE_INVALID_TRAILER = "Invalid trailing code unit";
    static immutable string UTF8_DECODE_INVALID_CODE_POINT = "Invalid code point decoded";

    /* The following encodings are valid, except for the 5 and 6 byte
     * combinations:
     *      0xxxxxxx
     *      110xxxxx 10xxxxxx
     *      1110xxxx 10xxxxxx 10xxxxxx
     *      11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
     *      111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
     *      1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
     */
    static immutable ubyte[256] UTF8_STRIDE =
    [
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,

        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,
        1,1,1,1, 1,1,1,1,

        0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
        0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
        0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
        0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
        0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
        0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
        0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
        0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,

        2,2,2,2, 2,2,2,2,
        2,2,2,2, 2,2,2,2,
        2,2,2,2, 2,2,2,2,
        2,2,2,2, 2,2,2,2,

        3,3,3,3, 3,3,3,3,
        3,3,3,3, 3,3,3,3,

        4,4,4,4, 4,4,4,4,
        5,5,5,5, 6,6,0xFF,0xFF
    ];

    assert(s !is null);
    size_t i = ridx++;

    const char u = s[i];
    // Pre-stage results for ASCII and error cases
    rresult = u;
    //printf("utf_decodeChar(s = %02x, %02x, %02x len = %d)\n", u, s[1], s[2], len);
    // Get expected sequence length
    const size_t n = UTF8_STRIDE[u];
    switch (n)
    {
    case 1:
        // ASCII
        return UTF8_DECODE_OK;
    case 2:
    case 3:
    case 4:
        // multi-byte UTF-8
        break;
    default:
        // 5- or 6-byte sequence
        return UTF8_DECODE_OUTSIDE_CODE_SPACE;
    }
    if (s.length < i + n) // source too short
        return UTF8_DECODE_TRUNCATED_SEQUENCE;
    // Pick off 7 - n low bits from first code unit
    dchar c = u & ((1 << (7 - n)) - 1);
    /* The following combinations are overlong, and illegal:
     *      1100000x (10xxxxxx)
     *      11100000 100xxxxx (10xxxxxx)
     *      11110000 1000xxxx (10xxxxxx 10xxxxxx)
     *      11111000 10000xxx (10xxxxxx 10xxxxxx 10xxxxxx)
     *      11111100 100000xx (10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx)
     */
    const char u2 = s[++i];
    // overlong combination
    if ((u & 0xFE) == 0xC0 || (u == 0xE0 && (u2 & 0xE0) == 0x80) || (u == 0xF0 && (u2 & 0xF0) == 0x80) || (u == 0xF8 && (u2 & 0xF8) == 0x80) || (u == 0xFC && (u2 & 0xFC) == 0x80))
        return UTF8_DECODE_OVERLONG;
    // Decode remaining bits
    for (const m = n + i - 1; i != m; ++i)
    {
        const u3 = s[i];
        if ((u3 & 0xC0) != 0x80) // trailing bytes are 10xxxxxx
            return UTF8_DECODE_INVALID_TRAILER;
        c = (c << 6) | (u3 & 0x3F);
    }
    if (!utf_isValidDchar(c))
        return UTF8_DECODE_INVALID_CODE_POINT;
    ridx = i;
    rresult = c;
    return UTF8_DECODE_OK;
}

/********************************************
 * Decode a UTF-16 sequence as a single UTF-32 code point.
 * Params:
 *      s = UTF-16 sequence
 *      ridx = starting index in s[], updated to reflect number of code units decoded
 *      rresult = set to character decoded
 * Returns:
 *      null on success, otherwise error message string
 */
string utf_decodeWchar(const(wchar)[] s, ref size_t ridx, out dchar rresult)
{
    // UTF-16 decoding errors
    static immutable string UTF16_DECODE_OK = null; // no error
    static immutable string UTF16_DECODE_TRUNCATED_SEQUENCE = "Truncated UTF-16 sequence";
    static immutable string UTF16_DECODE_INVALID_SURROGATE = "Invalid low surrogate";
    static immutable string UTF16_DECODE_UNPAIRED_SURROGATE = "Unpaired surrogate";
    static immutable string UTF16_DECODE_INVALID_CODE_POINT = "Invalid code point decoded";

    assert(s !is null);
    size_t i = ridx++;

    // Pre-stage results for single wchar and error cases
    dchar u = rresult = s[i];
    if (u < 0xD800) // Single wchar codepoint
        return UTF16_DECODE_OK;
    if (0xD800 <= u && u <= 0xDBFF) // Surrogate pair
    {
        if (s.length <= i + 1)
            return UTF16_DECODE_TRUNCATED_SEQUENCE;
        wchar u2 = s[i + 1];
        if (u2 < 0xDC00 || 0xDFFF < u)
            return UTF16_DECODE_INVALID_SURROGATE;
        u = ((u - 0xD7C0) << 10) + (u2 - 0xDC00);
        ++ridx;
    }
    else if (0xDC00 <= u && u <= 0xDFFF)
        return UTF16_DECODE_UNPAIRED_SURROGATE;
    if (!utf_isValidDchar(u))
        return UTF16_DECODE_INVALID_CODE_POINT;
    rresult = u;
    return UTF16_DECODE_OK;
}
