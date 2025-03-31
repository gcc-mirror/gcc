/**
 * Give import hints for common symbol names that couldn't be resolved.
 *
 * For example, prompt to `import std.stdio` when using `writeln`.
 *
 * Copyright:   Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/compiler/src/dmd/imphint.d, _imphint.d)
 * Documentation:  https://dlang.org/phobos/dmd_imphint.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/compiler/src/dmd/imphint.d
 */

module dmd.imphint;

/******************************************
 * Looks for undefined identifier s to see
 * if it might be undefined because an import
 * was not specified.
 * Not meant to be a comprehensive list of names in each module,
 * just the most common ones.
 */
const(char)[] importHint(const(char)[] s) @safe
{
    if (auto entry = s in hints)
        return *entry;
    return null;
}

private immutable string[string] hints;

shared static this()
{
    // in alphabetic order
    hints = [
        "AliasSeq": "std.meta",
        "appender": "std.array",
        "array": "std.array",
        "calloc": "core.stdc.stdlib",
        "chdir": "std.file",
        "cos": "std.math",
        "dirEntries": "std.file",
        "drop": "std.range",
        "each": "std.algorithm",
        "empty": "std.range",
        "endsWith": "std.algorithm",
        "enforce": "std.exception",
        "enumerate": "std.range",
        "equal": "std.algorithm",
        "exists": "std.file",
        "fabs": "std.math",
        "filter": "std.algorithm",
        "format": "std.format",
        "free": "core.stdc.stdlib",
        "front": "std.range",
        "iota": "std.range",
        "isDir": "std.file",
        "isFile": "std.file",
        "join": "std.array",
        "joiner": "std.algorithm",
        "malloc": "core.stdc.stdlib",
        "map": "std.algorithm",
        "max": "std.algorithm",
        "min": "std.algorithm",
        "mkdir": "std.file",
        "popFront": "std.range",
        "printf": "core.stdc.stdio",
        "realloc": "core.stdc.stdlib",
        "replace": "std.array",
        "rmdir": "std.file",
        "sin": "std.math",
        "sort": "std.algorithm",
        "split": "std.array",
        "sqrt": "std.math",
        "startsWith": "std.algorithm",
        "take": "std.range",
        "text": "std.conv",
        "to": "std.conv",
        "writefln": "std.stdio",
        "writeln": "std.stdio",
        "__va_argsave_t": "core.stdc.stdarg",
        "__va_list_tag": "core.stdc.stdarg",
        "InterpolationHeader": "core.interpolation",
        "InterpolationFooter": "core.interpolation",
    ];
}

unittest
{
    assert(importHint("printf") !is null);
    assert(importHint("fabs") !is null);
    assert(importHint("xxxxx") is null);
}
