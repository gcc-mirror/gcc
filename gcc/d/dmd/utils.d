/**
 * This module defines some utility functions for DMD.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/utils.d, _utils.d)
 * Documentation:  https://dlang.org/phobos/dmd_utils.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/utils.d
 */

module dmd.utils;

import core.stdc.string;
import dmd.errors;
import dmd.location;
import dmd.root.file;
import dmd.root.filename;
import dmd.common.outbuffer;
import dmd.root.string;

nothrow:

/**
 * Normalize path by turning forward slashes into backslashes
 *
 * Params:
 *   src = Source path, using unix-style ('/') path separators
 *
 * Returns:
 *   A newly-allocated string with '/' turned into backslashes
 */
const(char)* toWinPath(const(char)* src)
{
    if (src is null)
        return null;
    char* result = strdup(src);
    char* p = result;
    while (*p != '\0')
    {
        if (*p == '/')
            *p = '\\';
        p++;
    }
    return result;
}


/**
 * Reads a file, terminate the program on error
 *
 * Params:
 *   loc = The line number information from where the call originates
 *   filename = Path to file
 *   buf = append contents of file to
 * Returns:
 *   true on failure
 */
bool readFile(Loc loc, const(char)[] filename, ref OutBuffer buf)
{
    if (File.read(filename, buf))
    {
        error(loc, "error reading file `%.*s`", cast(int)filename.length, filename.ptr);
        return true;
    }
    return false;
}


/**
 * Writes a file, terminate the program on error
 *
 * Params:
 *   loc = The line number information from where the call originates
 *   filename = Path to file
 *   data = Full content of the file to be written
 * Returns:
 *   false on error
 */
extern (D) bool writeFile(Loc loc, const(char)[] filename, const void[] data)
{
    if (!ensurePathToNameExists(Loc.initial, filename))
        return false;
    if (!File.update(filename, data))
    {
        error(loc, "error writing file '%.*s'", cast(int) filename.length, filename.ptr);
        return false;
    }
    return true;
}


/**
 * Ensure the root path (the path minus the name) of the provided path
 * exists, and terminate the process if it doesn't.
 *
 * Params:
 *   loc = The line number information from where the call originates
 *   name = a path to check (the name is stripped)
 * Returns:
 *      false on error
 */
bool ensurePathToNameExists(Loc loc, const(char)[] name)
{
    const char[] pt = FileName.path(name);
    if (pt.length)
    {
        if (!FileName.ensurePathExists(pt))
        {
            error(loc, "cannot create directory %*.s", cast(int) pt.length, pt.ptr);
            FileName.free(pt.ptr);
            return false;
        }
    }
    FileName.free(pt.ptr);
    return true;
}


/**
 * Takes a path, and escapes '(', ')' and backslashes
 *
 * Params:
 *   buf = Buffer to write the escaped path to
 *   fname = Path to escape
 */
void escapePath(OutBuffer* buf, const(char)* fname) pure
{
    while (1)
    {
        switch (*fname)
        {
        case 0:
            return;
        case '(':
        case ')':
        case '\\':
            buf.writeByte('\\');
            goto default;
        default:
            buf.writeByte(*fname);
            break;
        }
        fname++;
    }
}

/**
 * Convert string to integer.
 *
 * Params:
 *  T = Type of integer to parse
 *  val = Variable to store the result in
 *  p = slice to start of string digits
 *  max = max allowable value (inclusive), defaults to `T.max`
 *
 * Returns:
 *  `false` on error, `true` on success
 */
bool parseDigits(T)(ref T val, const(char)[] p, const T max = T.max)
    @safe pure @nogc nothrow
{
    import core.checkedint : mulu, addu, muls, adds;

    // mul* / add* doesn't support types < int
    static if (T.sizeof < int.sizeof)
    {
        int value;
        alias add = adds;
        alias mul = muls;
    }
    // unsigned
    else static if (T.min == 0)
    {
        T value;
        alias add = addu;
        alias mul = mulu;
    }
    else
    {
        T value;
        alias add = adds;
        alias mul = muls;
    }

    bool overflow;
    foreach (char c; p)
    {
        if (c > '9' || c < '0')
            return false;
        value = mul(value, 10, overflow);
        value = add(value, uint(c - '0'), overflow);
    }
    // If it overflows, value must be > to `max` (since `max` is `T`)
    val = cast(T) value;
    return !overflow && value <= max;
}

///
@safe pure nothrow @nogc unittest
{
    byte b;
    ubyte ub;
    short s;
    ushort us;
    int i;
    uint ui;
    long l;
    ulong ul;

    assert(b.parseDigits("42") && b  == 42);
    assert(ub.parseDigits("42") && ub == 42);

    assert(s.parseDigits("420") && s  == 420);
    assert(us.parseDigits("42000") && us == 42_000);

    assert(i.parseDigits("420000") && i  == 420_000);
    assert(ui.parseDigits("420000") && ui == 420_000);

    assert(l.parseDigits("42000000000") && l  == 42_000_000_000);
    assert(ul.parseDigits("82000000000") && ul == 82_000_000_000);

    assert(!b.parseDigits(ubyte.max.stringof));
    assert(!b.parseDigits("WYSIWYG"));
    assert(!b.parseDigits("-42"));
    assert(!b.parseDigits("200"));
    assert(ub.parseDigits("200") && ub == 200);
    assert(i.parseDigits(int.max.stringof) && i == int.max);
    assert(i.parseDigits("420", 500) && i == 420);
    assert(!i.parseDigits("420", 400));
}

/**
 * Cast a `ubyte[]` to an array of larger integers as if we are on a big endian architecture
 * Params:
 *   data = array with big endian data
 *   size = 1 for ubyte[], 2 for ushort[], 4 for uint[], 8 for ulong[]
 * Returns: copy of `data`, with bytes shuffled if compiled for `version(LittleEndian)`
 */
ubyte[] arrayCastBigEndian(const ubyte[] data, size_t size) @safe
{
    ubyte[] impl(T)()
    {
        auto result = new T[](data.length / T.sizeof);
        foreach (i; 0 .. result.length)
        {
            result[i] = 0;
            foreach (j; 0 .. T.sizeof)
            {
                result[i] |= T(data[i * T.sizeof + j]) << ((T.sizeof - 1 - j) * 8);
            }
        }
        return cast(ubyte[]) result;
    }
    switch (size)
    {
        case 1: return data.dup;
        case 2: return impl!ushort;
        case 4: return impl!uint;
        case 8: return impl!ulong;
        default: assert(0);
    }
}

unittest
{
    ubyte[] data = [0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22];
    assert(cast(ulong[]) arrayCastBigEndian(data, 8) == [0xAABBCCDDEEFF1122]);
    assert(cast(uint[]) arrayCastBigEndian(data, 4) == [0xAABBCCDD, 0xEEFF1122]);
    assert(cast(ushort[]) arrayCastBigEndian(data, 2) == [0xAABB, 0xCCDD, 0xEEFF, 0x1122]);
    assert(cast(ubyte[]) arrayCastBigEndian(data, 1) == data);
}
