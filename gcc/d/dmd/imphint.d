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
const(char)[] cIncludeHint(const(char)[] s) @safe
{
    if (auto entry = s in cHints)
        return *entry;
    return null;
}

private immutable string[string] hints;
private immutable string[string] cHints;

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
    cHints = [
        "va_list": "<stdarg.h>",

        "bool": "<stdbool.h>",
        "false": "<stdbool.h>",
        "true": "<stdbool.h>",

        "NULL": "<stddef.h>",
        "nullptr_t": "<stddef.h>",
        "offsetof": "<stddef.h>",
        "ptrdiff_t": "<stddef.h>",
        "size_t": "<stddef.h>",
        "wchar_t": "<stddef.h>",

        "INT8_MAX": "<stdint.h>",
        "INT16_MAX": "<stdint.h>",
        "INT32_MAX": "<stdint.h>",
        "INT64_MAX": "<stdint.h>",
        "INTPTR_MAX": "<stdint.h>",
        "PTRDIFF_MAX": "<stdint.h>",
        "PTRDIFF_MIN": "<stdint.h>",
        "SIZE_MAX": "<stdint.h>",
        "UINT8_MAX": "<stdint.h>",
        "UINT16_MAX": "<stdint.h>",
        "UINT32_MAX": "<stdint.h>",
        "UINT64_MAX": "<stdint.h>",
        "UINTPTR_MAX": "<stdint.h>",
        "WINT_MAX": "<stdint.h>",
        "WINT_MIN": "<stdint.h>",
        "int8_t": "<stdint.h>",
        "int16_t": "<stdint.h>",
        "int32_t": "<stdint.h>",
        "int64_t": "<stdint.h>",
        "intptr_t": "<stdint.h>",
        "uint8_t": "<stdint.h>",
        "uint16_t": "<stdint.h>",
        "uint32_t": "<stdint.h>",
        "uint64_t": "<stdint.h>",
        "uintptr_t": "<stdint.h>",

        "EOF": "<stdio.h>",
        "FILE": "<stdio.h>",
        "fopen": "<stdio.h>",
        "fpos_t": "<stdio.h>",
        "fprintf": "<stdio.h>",
        "getchar": "<stdio.h>",
        "printf": "<stdio.h>",
        "snprintf": "<stdio.h>",
        "sprintf": "<stdio.h>",
        "stderr": "<stdio.h>",
        "stdin": "<stdio.h>",
        "stdout": "<stdio.h>",

        "EXIT_FAILURE": "<stdlib.h>",
        "EXIT_SUCCESS": "<stdlib.h>",
        "abort": "<stdlib.h>",
        "atexit": "<stdlib.h>",
        "calloc": "<stdlib.h>",
        "exit": "<stdlib.h>",
        "free": "<stdlib.h>",
        "getenv": "<stdlib.h>",
        "malloc": "<stdlib.h>",
        "realloc": "<stdlib.h>",

        "memchr": "<string.h>",
        "memcmp": "<string.h>",
        "memcpy": "<string.h>",
        "memmove": "<string.h>",
        "memset": "<string.h>",
        "strcat": "<string.h>",
        "strchr": "<string.h>",
        "strcmp": "<string.h>",
        "strcpy": "<string.h>",
        "strerror": "<string.h>",
        "strlen": "<string.h>",
        "strncat": "<string.h>",
        "strncmp": "<string.h>",
        "strncpy": "<string.h>",
        "strrchr": "<string.h>",
        "strspn": "<string.h>",
        "strstr": "<string.h>",

        "WCHAR_MAX": "<wchar.h>",
        "WCHAR_MIN": "<wchar.h>",
    ];
}

unittest
{
    assert(importHint("printf") !is null);
    assert(importHint("fabs") !is null);
    assert(importHint("xxxxx") is null);
}
