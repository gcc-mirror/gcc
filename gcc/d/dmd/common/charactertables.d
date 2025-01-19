/**
 * Character tables related to identifiers.
 *
 * Supports UAX31, C99, C11 and least restrictive (All).
 *
 * Copyright: Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:   $(LINK2 https://cattermole.co.nz, Richard (Rikki) Andrew Cattermole)
 * License:   $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/common/charactertables.d, common/charactertables.d)
 * Documentation: https://dlang.org/phobos/dmd_common_charactertables.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/common/charactertables.d
 */
module dmd.common.charactertables;

@safe nothrow @nogc pure:

extern(C++):

///
enum IdentifierTable {
    UAX31, ///
    C99, ///
    C11, ///
    LR, /// Least Restrictive aka All
}

///
struct IdentifierCharLookup
{
    @safe nothrow @nogc pure:

    ///
    extern(C++) bool function(dchar) isStart;
    ///
    extern(C++) bool function(dchar) isContinue;

    /// Lookup the table given the table name
    extern(C++) static IdentifierCharLookup forTable(IdentifierTable table)
    {
        import dmd.common.identifiertables;

        // Awful solution to require these lambdas.
        // However without them the extern(C++) ABI issues crop up for isInRange,
        //  and then it can't access the tables.
        final switch(table) {
            case IdentifierTable.UAX31:
                return IdentifierCharLookup(
                        (c) => isInRange!UAX31_Start(c),
                        (c) => isInRange!UAX31_Continue(c));
            case IdentifierTable.C99:
                return IdentifierCharLookup(
                        (c) => isInRange!FixedTable_C99_Start(c),
                        (c) => isInRange!FixedTable_C99_Continue(c));
            case IdentifierTable.C11:
                return IdentifierCharLookup(
                        (c) => isInRange!FixedTable_C11_Start(c),
                        (c) => isInRange!FixedTable_C11_Continue(c));
            case IdentifierTable.LR:
                return IdentifierCharLookup(
                        (c) => isInRange!LeastRestrictive_Start(c),
                        (c) => isInRange!LeastRestrictive_Continue(c));
        }
    }
}

/**
Convenience function for use in places where we just don't care,
what the identifier ranges are, or if it is start/continue.

Returns: is character a member of least restrictive of all.
*/
bool isAnyIdentifierCharacter(dchar c)
{
    import dmd.common.identifiertables;
    return isInRange!LeastRestrictive_OfAll(c);
}

///
unittest
{
    assert(isAnyIdentifierCharacter('ğ'));
}

/**
Convenience function for use in places where we just don't care,
what the identifier ranges are.

Returns: is character a member of restrictive Start
*/
bool isAnyStart(dchar c)
{
    import dmd.common.identifiertables;
    return isInRange!LeastRestrictive_Start(c);
}

///
unittest
{
    assert(isAnyStart('ğ'));
}

/**
Convenience function for use in places where we just don't care,
what the identifier ranges are.

Returns: is character a member of least restrictive Continue
*/
bool isAnyContinue(dchar c)
{
    import dmd.common.identifiertables;
    return isInRange!LeastRestrictive_Continue(c);
}

///
unittest
{
    assert(isAnyContinue('ğ'));
}

/// UTF line separator
enum LS = 0x2028;
/// UTF paragraph separator
enum PS = 0x2029;

private
{
    enum CMoctal  = 0x1;
    enum CMhex    = 0x2;
    enum CMidchar = 0x4;
    enum CMzerosecond = 0x8;
    enum CMdigitsecond = 0x10;
    enum CMsinglechar = 0x20;
}

///
bool isoctal(const char c)
{
    return (cmtable[c] & CMoctal) != 0;
}

///
bool ishex(const char c)
{
    return (cmtable[c] & CMhex) != 0;
}

///
bool isidchar(const char c)
{
    return (cmtable[c] & CMidchar) != 0;
}

///
bool isZeroSecond(const char c)
{
    return (cmtable[c] & CMzerosecond) != 0;
}

///
bool isDigitSecond(const char c)
{
    return (cmtable[c] & CMdigitsecond) != 0;
}

///
bool issinglechar(const char c)
{
    return (cmtable[c] & CMsinglechar) != 0;
}

///
bool c_isxdigit(const int c)
{
    return (( c >= '0' && c <= '9') ||
        ( c >= 'a' && c <= 'f') ||
        ( c >= 'A' && c <= 'F'));
}

///
bool c_isalnum(const int c)
{
    return (( c >= '0' && c <= '9') ||
        ( c >= 'a' && c <= 'z') ||
        ( c >= 'A' && c <= 'Z'));
}

extern(D) private:

// originally from dmd.root.utf
bool isInRange(alias Ranges)(dchar c)
{
    size_t high = Ranges.length - 1;
    // Shortcut search if c is out of range
    size_t low = (c < Ranges[0][0] || Ranges[high][1] < c) ? high + 1 : 0;
    // Binary search
    while (low <= high)
    {
        const size_t mid = low + ((high - low) >> 1);
        if (c < Ranges[mid][0])
            high = mid - 1;
        else if (Ranges[mid][1] < c)
            low = mid + 1;
        else
        {
            assert(Ranges[mid][0] <= c && c <= Ranges[mid][1]);
            return true;
        }
    }
    return false;
}

/********************************************
 * Do our own char maps
 */
// originally from dmd.lexer (was private)
static immutable cmtable = ()
{
    ubyte[256] table;
    foreach (const c; 0 .. table.length)
    {
        if ('0' <= c && c <= '7')
            table[c] |= CMoctal;
        if (c_isxdigit(c))
            table[c] |= CMhex;
        if (c_isalnum(c) || c == '_')
            table[c] |= CMidchar;

        switch (c)
        {
            case 'x': case 'X':
            case 'b': case 'B':
                table[c] |= CMzerosecond;
                break;

            case '0': .. case '9':
            case 'e': case 'E':
            case 'f': case 'F':
            case 'l': case 'L':
            case 'p': case 'P':
            case 'u': case 'U':
            case 'i':
            case '.':
            case '_':
                table[c] |= CMzerosecond | CMdigitsecond;
                break;

            default:
                break;
        }

        switch (c)
        {
            case '\\':
            case '\n':
            case '\r':
            case 0:
            case 0x1A:
            case '\'':
                break;
            default:
                if (!(c & 0x80))
                    table[c] |= CMsinglechar;
                break;
        }
    }
    return table;
}();
