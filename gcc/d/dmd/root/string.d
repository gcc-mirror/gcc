/**
 * Contains various string related functions.
 *
 * Copyright: Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:   Walter Bright, https://www.digitalmars.com
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/root/string.d, root/_string.d)
 * Documentation:  https://dlang.org/phobos/dmd_root_string.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/root/string.d
 */
module dmd.root.string;

import core.stdc.string;
import dmd.root.rmem;

/// Slices a `\0`-terminated C-string, excluding the terminator
inout(char)[] toDString (inout(char)* s) pure nothrow @nogc
{
    import core.stdc.string : strlen;
    return s ? s[0 .. strlen(s)] : null;
}

/**
Compare two slices for equality, in a case-insensitive way

Comparison is based on `char` and does not do decoding.
As a result, it's only really accurate for plain ASCII strings.

Params:
s1 = string to compare
s2 = string to compare

Returns:
`true` if `s1 == s2` regardless of case
*/
extern(D) static bool iequals(const(char)[] s1, const(char)[] s2) pure nothrow @nogc
{
    import core.stdc.ctype : toupper;

    if (s1.length != s2.length)
        return false;

    foreach (idx, c1; s1)
    {
        // Since we did a length check, it is safe to bypass bounds checking
        const c2 = s2.ptr[idx];
        if (c1 != c2)
            if (toupper(c1) != toupper(c2))
                return false;
    }
    return true;
}

/**
Copy the content of `src` into a C-string ('\0' terminated) then call `dg`

The intent of this function is to provide an allocation-less
way to call a C function using a D slice.
The function internally allocates a buffer if needed, but frees it on exit.

Note:
The argument to `dg` is `scope`. To keep the data around after `dg` exits,
one has to copy it.

Params:
src = Slice to use to call the C function
dg  = Delegate to call afterwards

Returns:
The return value of `T`
*/
auto toCStringThen(alias dg)(const(char)[] src) nothrow
{
    import dmd.root.rmem : mem;
    import dmd.common.smallbuffer : SmallBuffer;

    const len = src.length + 1;
    char[512] small = void;
    auto sb = SmallBuffer!char(len, small[]);
    scope ptr = sb[];
    ptr[0 .. src.length] = src[];
    ptr[src.length] = '\0';
    return dg(ptr);
}

unittest
{
    assert("Hello world".toCStringThen!((v) => v == "Hello world\0"));
    assert("Hello world\0".toCStringThen!((v) => v == "Hello world\0\0"));
    assert(null.toCStringThen!((v) => v == "\0"));
}

/*********************************************
 * Convert a D string to a C string by allocating memory,
 * copying it, and adding a terminating 0.
 * Params:
 *      s = string to copy
 * Result:
 *      0-terminated copy of s
 */
char[] toCString(scope const(char)[] s) nothrow
{
    const length = s.length;
    char* p = cast(char*)mem.xmalloc_noscan(length + 1);
    memcpy(p, s.ptr, length);
    p[length] = 0;
    return p[0 .. length];
}

/**
 * Strips one leading line terminator of the given string.
 *
 * The following are what the Unicode standard considers as line terminators:
 *
 * | Name                | D Escape Sequence | Unicode Code Point |
 * |---------------------|-------------------|--------------------|
 * | Line feed           | `\n`              | `U+000A`           |
 * | Line tabulation     | `\v`              | `U+000B`           |
 * | Form feed           | `\f`              | `U+000C`           |
 * | Carriage return     | `\r`              | `U+000D`           |
 * | Next line           |                   | `U+0085`           |
 * | Line separator      |                   | `U+2028`           |
 * | Paragraph separator |                   | `U+2029`           |
 *
 * This function will also strip `\r\n`.
 */
string stripLeadingLineTerminator(string str) pure nothrow @nogc @safe
{
    enum nextLine = "\xC2\x85";
    enum lineSeparator = "\xE2\x80\xA8";
    enum paragraphSeparator = "\xE2\x80\xA9";

    static assert(lineSeparator.length == paragraphSeparator.length);

    if (str.length == 0)
        return str;

    switch (str[0])
    {
        case '\r':
        {
            if (str.length >= 2 && str[1] == '\n')
                return str[2 .. $];
            goto case;
        }
        case '\v', '\f', '\n': return str[1 .. $];

        case nextLine[0]:
        {
            if (str.length >= 2 && str[0 .. 2] == nextLine)
                return str[2 .. $];

            return str;
        }

        case lineSeparator[0]:
        {
            if (str.length >= lineSeparator.length)
            {
                const prefix = str[0 .. lineSeparator.length];

                if (prefix == lineSeparator || prefix == paragraphSeparator)
                    return str[lineSeparator.length .. $];
            }

            return str;
        }

        default: return str;
    }
}

unittest
{
    assert("".stripLeadingLineTerminator == "");
    assert("foo".stripLeadingLineTerminator == "foo");
    assert("\xC2foo".stripLeadingLineTerminator == "\xC2foo");
    assert("\xE2foo".stripLeadingLineTerminator == "\xE2foo");
    assert("\nfoo".stripLeadingLineTerminator == "foo");
    assert("\vfoo".stripLeadingLineTerminator == "foo");
    assert("\ffoo".stripLeadingLineTerminator == "foo");
    assert("\rfoo".stripLeadingLineTerminator == "foo");
    assert("\u0085foo".stripLeadingLineTerminator == "foo");
    assert("\u2028foo".stripLeadingLineTerminator == "foo");
    assert("\u2029foo".stripLeadingLineTerminator == "foo");
    assert("\n\rfoo".stripLeadingLineTerminator == "\rfoo");
    assert("\r\nfoo".stripLeadingLineTerminator == "foo");
}

/**
 * A string comparison functions that returns the same result as strcmp
 *
 * Note: Strings are compared based on their ASCII values, no UTF-8 decoding.
 *
 * Some C functions (e.g. `qsort`) require a `int` result for comparison.
 * See_Also: Druntime's `core.internal.string`
 */
int dstrcmp()( scope const char[] s1, scope const char[] s2 ) @trusted
{
    immutable len = s1.length <= s2.length ? s1.length : s2.length;
    if (__ctfe)
    {
        foreach (const u; 0 .. len)
        {
            if (s1[u] != s2[u])
                return s1[u] > s2[u] ? 1 : -1;
        }
    }
    else
    {
        import core.stdc.string : memcmp;

        const ret = memcmp( s1.ptr, s2.ptr, len );
        if ( ret )
            return ret;
    }
    return s1.length < s2.length ? -1 : (s1.length > s2.length);
}

//
unittest
{
    assert(dstrcmp("Fraise", "Fraise")      == 0);
    assert(dstrcmp("Baguette", "Croissant") < 0);
    assert(dstrcmp("Croissant", "Baguette") > 0);

    static assert(dstrcmp("Baguette", "Croissant") < 0);

    // UTF-8 decoding for the CT variant
    assert(dstrcmp("안녕하세요!", "안녕하세요!") == 0);
    static assert(dstrcmp("안녕하세요!", "안녕하세요!") == 0);
}

/**
 * Infers the length `N` of a string literal and coerces its type to a static
 * array with length `N + 1`. Returns the string with a null character appended
 * to the end.
 *
 * Params:
 *  literal = string literal
 *
 * Notes:
 *  - LDC produces quite optimal code for short strings:
 *    - https://d.godbolt.org/z/M69Z1g
 *    - https://gist.github.com/PetarKirov/338e4ab9292b6b2b311a3070572a07fb (backup URL)
*/
char[N + 1] toStaticArray(size_t N)(scope const(char)[N] literal)
{
    char[N+1] result = void;
    result[0..N] = literal[0..N];
    result[N] = 0;
    return result;
}

///
@safe pure nothrow @nogc
unittest
{
    auto m = "123".toStaticArray;
    const c = "123".toStaticArray;
    immutable i = "123".toStaticArray;
    enum e = "123".toStaticArray;

    assert(m == "123\0");
    assert(c == "123\0");
    assert(i == "123\0");
    static assert(e == "123\0");

    const empty = "".toStaticArray;
    static assert(empty.length == 1);
    static assert(empty[0] == '\0');
}

/**
 * Checks if C string `p` starts with `needle`.
 * Params:
 *     p = the C string to check
 *     needle = the string to look for
 * Returns:
 *    `true` if `p` starts with `needle`
 */
@system pure nothrow @nogc
bool startsWith(scope const(char)* p, scope const(char)[] needle)
in { assert(p && needle.ptr); }
do
{
    foreach (const c; needle)
    {
        assert(c);
        if (c != *p)
            return false;
        ++p;
    }
    return true;
}

///ditto
nothrow @nogc pure @safe
bool startsWith(scope const(char)[] str, scope const(char)[] prefix)
{
    if (str.length < prefix.length)
        return false;
    return str[0 .. prefix.length] == prefix;
}

///
@system pure nothrow @nogc
unittest
{
    const buf = "123".toStaticArray;
    const ptr = &buf[0];
    assert(ptr.startsWith(""));
    assert(ptr.startsWith("1"));
    assert(ptr.startsWith("12"));
    assert(ptr.startsWith("123"));
    assert(!ptr.startsWith("1234"));
}

/**********************************
 * Take `text` and turn it into an InputRange that emits
 * slices into `text` for each line.
 * Params:
 *  text = array of characters
 * Returns:
 *  InputRange accessing `text` as a sequence of lines
 * Reference:
 *  `std.string.splitLines()`
 */
auto splitLines(const char[] text)
{
    struct Range
    {
      @safe:
      @nogc:
      nothrow:
      pure:
      private:

        const char[] text;
        size_t index;       // index of start of line
        size_t eolIndex;    // index of end of line before newline characters
        size_t nextIndex;   // index past end of line

        public this(const char[] text)
        {
            this.text = text;
        }

        public bool empty() { return index == text.length; }

        public void popFront() { advance(); index = nextIndex; }

        public const(char)[] front() { advance(); return text[index .. eolIndex]; }

        private void advance()
        {
            if (index != nextIndex) // if already advanced
                return;

            for (size_t i = index; i < text.length; ++i)
            {
                switch (text[i])
                {
                    case '\v', '\f', '\n':
                        eolIndex = i;
                        nextIndex = i + 1;
                        return;

                    case '\r':
                        if (i + 1 < text.length && text[i + 1] == '\n') // decode "\r\n"
                        {
                            eolIndex = i;
                            nextIndex = i + 2;
                            return;
                        }
                        eolIndex = i;
                        nextIndex = i + 1;
                        return;

                    /* Manually decode:
                     *  NEL is C2 85
                     */
                    case 0xC2:
                        if (i + 1 < text.length && text[i + 1] == 0x85)
                        {
                            eolIndex = i;
                            nextIndex = i + 2;
                            return;
                        }
                        break;

                    /* Manually decode:
                     *  lineSep is E2 80 A8
                     *  paraSep is E2 80 A9
                     */
                    case 0xE2:
                        if (i + 2 < text.length &&
                            text[i + 1] == 0x80 &&
                            (text[i + 2] == 0xA8 || text[i + 2] == 0xA9)
                           )
                        {
                            eolIndex = i;
                            nextIndex = i + 3;
                            return;
                        }
                        break;

                    default:
                        break;
                }
            }
        }
    }

    return Range(text);
}

private struct FindSplit
{
@nogc nothrow pure @safe:
    const(char)[][3] elem;

    ref const(char)[] opIndex(size_t i) scope return { return elem[i]; }
    bool opCast() const scope { return elem[1].length > 0; }
}

/**
Find a substring in a string and split the string into before and after parts.
Params:
  str = string to look into
  needle = substring to find in str (must not be empty)
Returns:
   a `FindSplit` object that casts to `true` iff `needle` was found inside `str`.
   In that case, `split[1]` is the needle, and `split[0]`/`split[2]` are before/after the needle.
*/
FindSplit findSplit(return scope const(char)[] str, scope const(char)[] needle)
{
    if (needle.length > str.length)
        return FindSplit([str, null, null]);

    foreach (i; 0 .. str.length - needle.length + 1)
    {
        if (str[i .. i+needle.length] == needle[])
            return FindSplit([ str[0 .. i], str[i .. i+needle.length], str[i+needle.length .. $] ]);
    }
    return FindSplit([str, null, null]);
}

unittest
{
    auto s = findSplit("a b c", "c");
    assert(s[0] == "a b ");
    assert(s[1] == "c");
    assert(s[2] == "");
    auto s1 = findSplit("a b c", "b");
    assert(s1[0] == "a ");
    assert(s1[1] == "b");
    assert(s1[2] == " c");
    assert(!findSplit("a b c", "d"));
    assert(!findSplit("", "d"));
}

/**
Find a string inbetween two substrings
Params:
  str = string to look into
  l = substring to find on the left
  r = substring to find on the right
Returns:
   substring of `str` inbetween `l` and `r`
*/
const(char)[] findBetween(const(char)[] str, const(char)[] l, const(char)[] r)
{
    if (auto s0 = str.findSplit(l))
        if (auto s1 = s0[2].findSplit(r))
            return s1[0];
    return null;
}

unittest
{
    assert(findBetween("a b c", "a ", " c") == "b");
    assert(findBetween("a b c", "a ", " d") == null);
}
