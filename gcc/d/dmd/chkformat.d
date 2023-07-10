/**
 * Check the arguments to `printf` and `scanf` against the `format` string.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/chkformat.d, _chkformat.d)
 * Documentation:  https://dlang.org/phobos/dmd_chkformat.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/chkformat.d
 */
module dmd.chkformat;

//import core.stdc.stdio : printf, scanf;
import core.stdc.ctype : isdigit;

import dmd.astenums;
import dmd.cond;
import dmd.errors;
import dmd.expression;
import dmd.globals;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.target;


/******************************************
 * Check that arguments to a printf format string are compatible
 * with that string. Issue errors for incompatibilities.
 *
 * Follows the C99 specification for printf.
 *
 * Takes a generous, rather than strict, view of compatiblity.
 * For example, an unsigned value can be formatted with a signed specifier.
 *
 * Diagnosed incompatibilities are:
 *
 * 1. incompatible sizes which will cause argument misalignment
 * 2. deferencing arguments that are not pointers
 * 3. insufficient number of arguments
 * 4. struct arguments
 * 5. array and slice arguments
 * 6. non-pointer arguments to `s` specifier
 * 7. non-standard formats
 * 8. undefined behavior per C99
 *
 * Per the C Standard, extra arguments are ignored.
 *
 * No attempt is made to fix the arguments or the format string.
 *
 * Params:
 *      loc = location for error messages
 *      format = format string
 *      args = arguments to match with format string
 *      isVa_list = if a "v" function (format check only)
 *
 * Returns:
 *      `true` if errors occurred
 * References:
 * C99 7.19.6.1
 * https://www.cplusplus.com/reference/cstdio/printf/
 */
bool checkPrintfFormat(ref const Loc loc, scope const char[] format, scope Expression[] args, bool isVa_list)
{
    //printf("checkPrintFormat('%.*s')\n", cast(int)format.length, format.ptr);
    size_t n;    // index in args
    for (size_t i = 0; i < format.length;)
    {
        if (format[i] != '%')
        {
            ++i;
            continue;
        }
        bool widthStar;
        bool precisionStar;
        size_t j = i;
        const fmt = parsePrintfFormatSpecifier(format, j, widthStar, precisionStar);
        const slice = format[i .. j];
        i = j;

        if (fmt == Format.percent)
            continue;                   // "%%", no arguments
        if (fmt == Format.GNU_m)
            continue;                   // "%m", no arguments

        if (isVa_list)
        {
            // format check only
            if (fmt == Format.error)
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", cast(int)slice.length, slice.ptr);
            continue;
        }

        Expression getNextArg(ref bool skip)
        {
            if (n == args.length)
            {
                if (args.length < (n + 1))
                    deprecation(loc, "more format specifiers than %d arguments", cast(int)n);
                else
                    skip = true;
                return null;
            }
            return args[n++];
        }

        void errorMsg(const char* prefix, Expression arg, const char* texpect, Type tactual)
        {
            deprecation(arg.loc, "%sargument `%s` for format specification `\"%.*s\"` must be `%s`, not `%s`",
                  prefix ? prefix : "", arg.toChars(), cast(int)slice.length, slice.ptr, texpect, tactual.toChars());
        }

        if (widthStar)
        {
            bool skip;
            auto e = getNextArg(skip);
            if (skip)
                continue;
            if (!e)
                return true;
            auto t = e.type.toBasetype();
            if (t.ty != Tint32 && t.ty != Tuns32)
                errorMsg("width ", e, "int", t);
        }

        if (precisionStar)
        {
            bool skip;
            auto e = getNextArg(skip);
            if (skip)
                continue;
            if (!e)
                return true;
            auto t = e.type.toBasetype();
            if (t.ty != Tint32 && t.ty != Tuns32)
                errorMsg("precision ", e, "int", t);
        }

        bool skip;
        auto e = getNextArg(skip);
        if (skip)
            continue;
        if (!e)
            return true;
        auto t = e.type.toBasetype();
        auto tnext = t.nextOf();
        const c_longsize = target.c.longsize;
        const ptrsize = target.ptrsize;

        // Types which are promoted to int are allowed.
        // Spec: C99 6.5.2.2.7
        final switch (fmt)
        {
            case Format.u:      // unsigned int
            case Format.d:      // int
                if (t.ty != Tint32 && t.ty != Tuns32)
                    errorMsg(null, e, fmt == Format.u ? "uint" : "int", t);
                break;

            case Format.hhu:    // unsigned char
            case Format.hhd:    // signed char
                if (t.ty != Tint32 && t.ty != Tuns32 && t.ty != Tint8 && t.ty != Tuns8)
                    errorMsg(null, e, fmt == Format.hhu ? "ubyte" : "byte", t);
                break;

            case Format.hu:     // unsigned short int
            case Format.hd:     // short int
                if (t.ty != Tint32 && t.ty != Tuns32 && t.ty != Tint16 && t.ty != Tuns16)
                    errorMsg(null, e, fmt == Format.hu ? "ushort" : "short", t);
                break;

            case Format.lu:     // unsigned long int
            case Format.ld:     // long int
                if (!(t.isintegral() && t.size() == c_longsize))
                {
                    if (fmt == Format.lu)
                        errorMsg(null, e, (c_longsize == 4 ? "uint" : "ulong"), t);
                    else
                        errorMsg(null, e, (c_longsize == 4 ? "int" : "long"), t);
                    if (t.isintegral() && t.size() != c_longsize)
                        errorSupplemental(e.loc, "C `long` is %d bytes on your system", c_longsize);
                }
                break;

            case Format.llu:    // unsigned long long int
            case Format.lld:    // long long int
                if (t.ty != Tint64 && t.ty != Tuns64)
                    errorMsg(null, e, fmt == Format.llu ? "ulong" : "long", t);
                break;

            case Format.ju:     // uintmax_t
            case Format.jd:     // intmax_t
                if (t.ty != Tint64 && t.ty != Tuns64)
                {
                    if (fmt == Format.ju)
                        errorMsg(null, e, "core.stdc.stdint.uintmax_t", t);
                    else
                        errorMsg(null, e, "core.stdc.stdint.intmax_t", t);
                }
                break;

            case Format.zd:     // size_t
                if (!(t.isintegral() && t.size() == ptrsize))
                    errorMsg(null, e, "size_t", t);
                break;

            case Format.td:     // ptrdiff_t
                if (!(t.isintegral() && t.size() == ptrsize))
                    errorMsg(null, e, "ptrdiff_t", t);
                break;

            case Format.lg:
            case Format.g:      // double
                if (t.ty != Tfloat64 && t.ty != Timaginary64)
                    errorMsg(null, e, "double", t);
                break;

            case Format.Lg:     // long double
                if (t.ty != Tfloat80 && t.ty != Timaginary80)
                    errorMsg(null, e, "real", t);
                break;

            case Format.p:      // pointer
                if (t.ty != Tpointer && t.ty != Tnull && t.ty != Tclass && t.ty != Tdelegate && t.ty != Taarray)
                    errorMsg(null, e, "void*", t);
                break;

            case Format.n:      // pointer to int
                if (!(t.ty == Tpointer && tnext.ty == Tint32))
                    errorMsg(null, e, "int*", t);
                break;

            case Format.ln:     // pointer to long int
                if (!(t.ty == Tpointer && tnext.isintegral() && tnext.size() == c_longsize))
                    errorMsg(null, e, (c_longsize == 4 ? "int*" : "long*"), t);
                break;

            case Format.lln:    // pointer to long long int
                if (!(t.ty == Tpointer && tnext.ty == Tint64))
                    errorMsg(null, e, "long*", t);
                break;

            case Format.hn:     // pointer to short
                if (!(t.ty == Tpointer && tnext.ty == Tint16))
                    errorMsg(null, e, "short*", t);
                break;

            case Format.hhn:    // pointer to signed char
                if (!(t.ty == Tpointer && tnext.ty == Tint16))
                    errorMsg(null, e, "byte*", t);
                break;

            case Format.jn:     // pointer to intmax_t
                if (!(t.ty == Tpointer && tnext.ty == Tint64))
                    errorMsg(null, e, "core.stdc.stdint.intmax_t*", t);
                break;

            case Format.zn:     // pointer to size_t
                if (!(t.ty == Tpointer && tnext.isintegral() && tnext.isunsigned() && tnext.size() == ptrsize))
                    errorMsg(null, e, "size_t*", t);
                break;

            case Format.tn:     // pointer to ptrdiff_t
                if (!(t.ty == Tpointer && tnext.isintegral() && !tnext.isunsigned() && tnext.size() == ptrsize))
                    errorMsg(null, e, "ptrdiff_t*", t);
                break;

            case Format.c:      // char
                if (t.ty != Tint32 && t.ty != Tuns32)
                    errorMsg(null, e, "char", t);
                break;

            case Format.lc:     // wint_t
                if (t.ty != Tint32 && t.ty != Tuns32)
                    errorMsg(null, e, "wchar_t", t);
                break;

            case Format.s:      // pointer to char string
                if (!(t.ty == Tpointer && (tnext.ty == Tchar || tnext.ty == Tint8 || tnext.ty == Tuns8)))
                    errorMsg(null, e, "char*", t);
                break;

            case Format.ls:     // pointer to wchar_t string
                if (!(t.ty == Tpointer && tnext.ty.isSomeChar && tnext.size() == target.c.wchar_tsize))
                    errorMsg(null, e, "wchar_t*", t);
                break;

            case Format.error:
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", cast(int)slice.length, slice.ptr);
                break;

            case Format.GNU_m:
            case Format.POSIX_ms:
            case Format.POSIX_mls:
            case Format.percent:
                assert(0);
        }
    }
    return false;
}

/******************************************
 * Check that arguments to a scanf format string are compatible
 * with that string. Issue errors for incompatibilities.
 *
 * Follows the C99 specification for scanf.
 *
 * Takes a generous, rather than strict, view of compatiblity.
 * For example, an unsigned value can be formatted with a signed specifier.
 *
 * Diagnosed incompatibilities are:
 *
 * 1. incompatible sizes which will cause argument misalignment
 * 2. deferencing arguments that are not pointers
 * 3. insufficient number of arguments
 * 4. struct arguments
 * 5. array and slice arguments
 * 6. non-standard formats
 * 7. undefined behavior per C99
 *
 * Per the C Standard, extra arguments are ignored.
 *
 * No attempt is made to fix the arguments or the format string.
 *
 * Params:
 *      loc = location for error messages
 *      format = format string
 *      args = arguments to match with format string
 *      isVa_list = if a "v" function (format check only)
 *
 * Returns:
 *      `true` if errors occurred
 * References:
 * C99 7.19.6.2
 * https://www.cplusplus.com/reference/cstdio/scanf/
 */
bool checkScanfFormat(ref const Loc loc, scope const char[] format, scope Expression[] args, bool isVa_list)
{
    size_t n = 0;
    for (size_t i = 0; i < format.length;)
    {
        if (format[i] != '%')
        {
            ++i;
            continue;
        }
        bool asterisk;
        size_t j = i;
        const fmt = parseScanfFormatSpecifier(format, j, asterisk);
        const slice = format[i .. j];
        i = j;

        if (fmt == Format.percent || asterisk)
            continue;   // "%%", "%*": no arguments

        if (isVa_list)
        {
            // format check only
            if (fmt == Format.error)
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", cast(int)slice.length, slice.ptr);
            continue;
        }

        Expression getNextArg()
        {
            if (n == args.length)
            {
                if (!asterisk)
                    deprecation(loc, "more format specifiers than %d arguments", cast(int)n);
                return null;
            }
            return args[n++];
        }

        void errorMsg(const char* prefix, Expression arg, const char* texpect, Type tactual)
        {
            deprecation(arg.loc, "%sargument `%s` for format specification `\"%.*s\"` must be `%s`, not `%s`",
                  prefix ? prefix : "", arg.toChars(), cast(int)slice.length, slice.ptr, texpect, tactual.toChars());
        }

        auto e = getNextArg();
        if (!e)
            return true;

        auto t = e.type.toBasetype();
        auto tnext = t.nextOf();
        const c_longsize = target.c.longsize;
        const ptrsize = target.ptrsize;

        final switch (fmt)
        {
            case Format.n:
            case Format.d:      // pointer to int
                if (!(t.ty == Tpointer && tnext.ty == Tint32))
                    errorMsg(null, e, "int*", t);
                break;

            case Format.hhn:
            case Format.hhd:    // pointer to signed char
                if (!(t.ty == Tpointer && tnext.ty == Tint16))
                    errorMsg(null, e, "byte*", t);
                break;

            case Format.hn:
            case Format.hd:     // pointer to short
                if (!(t.ty == Tpointer && tnext.ty == Tint16))
                    errorMsg(null, e, "short*", t);
                break;

            case Format.ln:
            case Format.ld:     // pointer to long int
                if (!(t.ty == Tpointer && tnext.isintegral() && !tnext.isunsigned() && tnext.size() == c_longsize))
                    errorMsg(null, e, (c_longsize == 4 ? "int*" : "long*"), t);
                break;

            case Format.lln:
            case Format.lld:    // pointer to long long int
                if (!(t.ty == Tpointer && tnext.ty == Tint64))
                    errorMsg(null, e, "long*", t);
                break;

            case Format.jn:
            case Format.jd:     // pointer to intmax_t
                if (!(t.ty == Tpointer && tnext.ty == Tint64))
                    errorMsg(null, e, "core.stdc.stdint.intmax_t*", t);
                break;

            case Format.zn:
            case Format.zd:     // pointer to size_t
                if (!(t.ty == Tpointer && tnext.isintegral() && tnext.isunsigned() && tnext.size() == ptrsize))
                    errorMsg(null, e, "size_t*", t);
                break;

            case Format.tn:
            case Format.td:     // pointer to ptrdiff_t
                if (!(t.ty == Tpointer && tnext.isintegral() && !tnext.isunsigned() && tnext.size() == ptrsize))
                    errorMsg(null, e, "ptrdiff_t*", t);
                break;

            case Format.u:      // pointer to unsigned int
                if (!(t.ty == Tpointer && tnext.ty == Tuns32))
                    errorMsg(null, e, "uint*", t);
                break;

            case Format.hhu:    // pointer to unsigned char
                if (!(t.ty == Tpointer && tnext.ty == Tuns8))
                    errorMsg(null, e, "ubyte*", t);
                break;

            case Format.hu:     // pointer to unsigned short int
                if (!(t.ty == Tpointer && tnext.ty == Tuns16))
                    errorMsg(null, e, "ushort*", t);
                break;

            case Format.lu:     // pointer to unsigned long int
                if (!(t.ty == Tpointer && tnext.isintegral() && tnext.isunsigned() && tnext.size() == c_longsize))
                    errorMsg(null, e, (c_longsize == 4 ? "uint*" : "ulong*"), t);
                break;

            case Format.llu:    // pointer to unsigned long long int
                if (!(t.ty == Tpointer && tnext.ty == Tuns64))
                    errorMsg(null, e, "ulong*", t);
                break;

            case Format.ju:     // pointer to uintmax_t
                if (!(t.ty == Tpointer && tnext.ty == Tuns64))
                    errorMsg(null, e, "core.stdc.stdint.uintmax_t*", t);
                break;

            case Format.g:      // pointer to float
                if (!(t.ty == Tpointer && tnext.ty == Tfloat32))
                    errorMsg(null, e, "float*", t);
                break;

            case Format.lg:     // pointer to double
                if (!(t.ty == Tpointer && tnext.ty == Tfloat64))
                    errorMsg(null, e, "double*", t);
                break;

            case Format.Lg:     // pointer to long double
                if (!(t.ty == Tpointer && tnext.ty == Tfloat80))
                    errorMsg(null, e, "real*", t);
                break;

            case Format.c:
            case Format.s:      // pointer to char string
                if (!(t.ty == Tpointer && (tnext.ty == Tchar || tnext.ty == Tint8 || tnext.ty == Tuns8)))
                    errorMsg(null, e, "char*", t);
                break;

            case Format.lc:
            case Format.ls:     // pointer to wchar_t string
                if (!(t.ty == Tpointer && tnext.ty.isSomeChar && tnext.size() == target.c.wchar_tsize))
                    errorMsg(null, e, "wchar_t*", t);
                break;

            case Format.p:      // double pointer
                if (!(t.ty == Tpointer && tnext.ty == Tpointer))
                    errorMsg(null, e, "void**", t);
                break;

            case Format.POSIX_ms: // pointer to pointer to char string
                Type tnext2 = tnext ? tnext.nextOf() : null;
                if (!(t.ty == Tpointer && tnext.ty == Tpointer && (tnext2.ty == Tchar || tnext2.ty == Tint8 || tnext2.ty == Tuns8)))
                    errorMsg(null, e, "char**", t);
                break;

            case Format.POSIX_mls: // pointer to pointer to wchar_t string
                Type tnext2 = tnext ? tnext.nextOf() : null;
                if (!(t.ty == Tpointer && tnext.ty == Tpointer && tnext2.ty.isSomeChar && tnext2.size() == target.c.wchar_tsize))
                    errorMsg(null, e, "wchar_t**", t);
                break;

            case Format.error:
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", cast(int)slice.length, slice.ptr);
                break;

            case Format.GNU_m:
            case Format.percent:
                assert(0);
        }
    }
    return false;
}

private:

/**************************************
 * Parse the *format specifier* which is of the form:
 *
 * `%[*][width][length]specifier`
 *
 * Params:
 *      format = format string
 *      idx = index of `%` of start of format specifier,
 *          which gets updated to index past the end of it,
 *          even if `Format.error` is returned
 *      asterisk = set if there is a `*` sub-specifier
 * Returns:
 *      Format
 */
Format parseScanfFormatSpecifier(scope const char[] format, ref size_t idx,
        out bool asterisk) nothrow pure @safe
{
    auto i = idx;
    assert(format[i] == '%');
    const length = format.length;

    Format error()
    {
        idx = i;
        return Format.error;
    }

    ++i;
    if (i == length)
        return error();

    if (format[i] == '%')
    {
        idx = i + 1;
        return Format.percent;
    }

    // * sub-specifier
    if (format[i] == '*')
    {
        ++i;
        if (i == length)
            return error();
        asterisk = true;
    }

    // fieldWidth
    while (isdigit(format[i]))
    {
        i++;
        if (i == length)
            return error();
    }

    /* Read the specifier
     */
    Format specifier;
    Modifier flags = Modifier.none;
    switch (format[i])
    {
        case 'm':
            // https://pubs.opengroup.org/onlinepubs/9699919799/functions/scanf.html
            // POSIX.1-2017 C Extension (CX)
            flags = Modifier.m;
            ++i;
            if (i == length)
                return error();
            if (format[i] == 'l')
            {
                ++i;
                if (i == length)
                    return error();
                flags = Modifier.ml;
            }

            // Check valid conversion types for %m.
            if (format[i] == 'c' || format[i] == 's')
                specifier = flags == Modifier.ml ? Format.POSIX_mls :
                                                   Format.POSIX_ms;
            else if (format[i] == 'C' || format[i] == 'S')
                specifier = flags == Modifier.m ? Format.POSIX_mls :
                                                  Format.error;
            else if (format[i] == '[')
                goto case '[';
            else
                specifier = Format.error;
            ++i;
            break;

        case 'l':
            // Look for wchar_t scanset %l[..]
            immutable j = i + 1;
            if (j < length && format[j] == '[')
            {
                i = j;
                flags = Modifier.l;
                goto case '[';
            }
            goto default;

        case '[':
            // Read the scanset
            i++;
            if (i == length)
                return error();
            // If the conversion specifier begins with `[]` or `[^]`, the right
            // bracket character is not the terminator, but in the scanlist.
            if (format[i] == '^')
            {
                i++;
                if (i == length)
                    return error();
            }
            if (format[i] == ']')
            {
                i++;
                if (i == length)
                    return error();
            }
            // A scanset can be anything, so we just check that it is paired
            while (i < length)
            {
                if (format[i] == ']')
                    break;
                ++i;
            }
            // no `]` found
            if (i == length)
                return error();

            specifier = flags == Modifier.none ? Format.s         :
                        flags == Modifier.l    ? Format.ls        :
                        flags == Modifier.m    ? Format.POSIX_ms  :
                        flags == Modifier.ml   ? Format.POSIX_mls :
                                                 Format.error;
            ++i;
            break;

        default:
            char genSpec;
            specifier = parseGenericFormatSpecifier(format, i, genSpec);
            if (specifier == Format.error)
                return error();
            break;
    }

    idx = i;
    return specifier;  // success
}

/**************************************
 * Parse the *format specifier* which is of the form:
 *
 * `%[flags][field width][.precision][length modifier]specifier`
 *
 * Params:
 *      format = format string
 *      idx = index of `%` of start of format specifier,
 *          which gets updated to index past the end of it,
 *          even if `Format.error` is returned
 *      widthStar = set if * for width
 *      precisionStar = set if * for precision
 *      useGNUExts = true if parsing GNU format extensions
 * Returns:
 *      Format
 */
Format parsePrintfFormatSpecifier(scope const char[] format, ref size_t idx,
        out bool widthStar, out bool precisionStar, bool useGNUExts =
        findCondition(global.versionids, Identifier.idPool("CRuntime_Glibc"))) nothrow pure @safe
{
    auto i = idx;
    assert(format[i] == '%');
    const length = format.length;
    bool hash;
    bool zero;
    bool flags;
    bool width;
    bool precision;

    Format error()
    {
        idx = i;
        return Format.error;
    }

    ++i;
    if (i == length)
        return error();

    if (format[i] == '%')
    {
        idx = i + 1;
        return Format.percent;
    }

    /* Read the `flags`
     */
    while (1)
    {
        const c = format[i];
        if (c == '-' ||
            c == '+' ||
            c == ' ')
        {
            flags = true;
        }
        else if (c == '#')
        {
            hash = true;
        }
        else if (c == '0')
        {
            zero = true;
        }
        else
            break;
        ++i;
        if (i == length)
            return error();
    }

    /* Read the `field width`
     */
    {
        const c = format[i];
        if (c == '*')
        {
            width = true;
            widthStar = true;
            ++i;
            if (i == length)
                return error();
        }
        else if ('1' <= c && c <= '9')
        {
            width = true;
            ++i;
            if (i == length)
                return error();
            while ('0' <= format[i] && format[i] <= '9')
            {
                ++i;
                if (i == length)
                    return error();
            }
        }
    }

    /* Read the `precision`
     */
    if (format[i] == '.')
    {
        precision = true;
        ++i;
        if (i == length)
            return error();
        const c = format[i];
        if (c == '*')
        {
            precisionStar = true;
            ++i;
            if (i == length)
                return error();
        }
        else if ('0' <= c && c <= '9')
        {
            ++i;
            if (i == length)
                return error();
            while ('0' <= format[i] && format[i] <= '9')
            {
                ++i;
                if (i == length)
                    return error();
            }
        }
    }

    /* Read the specifier
     */
    char genSpec;
    Format specifier;
    switch (format[i])
    {
        case 'm':
            // https://www.gnu.org/software/libc/manual/html_node/Other-Output-Conversions.html
            if (useGNUExts)
            {
                specifier = Format.GNU_m;
                genSpec = format[i];
                ++i;
                break;
            }
            goto default;

        default:
            specifier = parseGenericFormatSpecifier(format, i, genSpec);
            if (specifier == Format.error)
                return error();
            break;
    }

    switch (genSpec)
    {
        case 'c':
        case 's':
        case 'C':
        case 'S':
            if (hash || zero)
                return error();
            break;

        case 'd':
        case 'i':
            if (hash)
                return error();
            break;

        case 'm':
            if (hash || zero || flags)
                return error();
            break;

        case 'n':
            if (hash || zero || precision || width || flags)
                return error();
            break;

        default:
            break;
    }

    idx = i;
    return specifier;  // success
}

/* Different kinds of conversion modifiers. */
enum Modifier
{
    none,
    h,          // short
    hh,         // char
    j,          // intmax_t
    l,          // wint_t/wchar_t
    ll,         // long long int
    L,          // long double
    m,          // char**
    ml,         // wchar_t**
    t,          // ptrdiff_t
    z           // size_t
}

/* Different kinds of formatting specifications, variations we don't
   care about are merged. (Like we don't care about the difference between
   f, e, g, a, etc.)

   For `scanf`, every format is a pointer.
 */
enum Format
{
    d,          // int
    hhd,        // signed char
    hd,         // short int
    ld,         // long int
    lld,        // long long int
    jd,         // intmax_t
    zd,         // size_t
    td,         // ptrdiff_t
    u,          // unsigned int
    hhu,        // unsigned char
    hu,         // unsigned short int
    lu,         // unsigned long int
    llu,        // unsigned long long int
    ju,         // uintmax_t
    g,          // float (scanf) / double (printf)
    lg,         // double (scanf)
    Lg,         // long double (both)
    s,          // char string (both)
    ls,         // wchar_t string (both)
    c,          // char (printf)
    lc,         // wint_t (printf)
    p,          // pointer
    n,          // pointer to int
    hhn,        // pointer to signed char
    hn,         // pointer to short
    ln,         // pointer to long int
    lln,        // pointer to long long int
    jn,         // pointer to intmax_t
    zn,         // pointer to size_t
    tn,         // pointer to ptrdiff_t
    GNU_m,      // GNU ext. : string corresponding to the error code in errno (printf)
    POSIX_ms,   // POSIX ext. : dynamically allocated char string  (scanf)
    POSIX_mls,  // POSIX ext. : dynamically allocated wchar_t string (scanf)
    percent,    // %% (i.e. no argument)
    error,      // invalid format specification
}

/**************************************
 * Parse the *length specifier* and the *specifier* of the following form:
 * `[length]specifier`
 *
 * Params:
 *      format = format string
 *      idx = index of of start of format specifier,
 *          which gets updated to index past the end of it,
 *          even if `Format.error` is returned
 *      genSpecifier = Generic specifier. For instance, it will be set to `d` if the
 *           format is `hdd`.
 * Returns:
 *      Format
 */
Format parseGenericFormatSpecifier(scope const char[] format,
    ref size_t idx, out char genSpecifier) nothrow pure @safe
{
    const length = format.length;

    /* Read the `length modifier`
     */
    const lm = format[idx];
    Modifier flags;
    switch (lm)
    {
        case 'j':
        case 'z':
        case 't':
        case 'L':
            flags = lm == 'j' ? Modifier.j :
                    lm == 'z' ? Modifier.z :
                    lm == 't' ? Modifier.t :
                                Modifier.L;
            ++idx;
            if (idx == length)
                return Format.error;
            break;

        case 'h':
        case 'l':
            ++idx;
            if (idx == length)
                return Format.error;
            if (lm == format[idx])
            {
                flags = lm == 'h' ? Modifier.hh : Modifier.ll;
                ++idx;
                if (idx == length)
                    return Format.error;
            }
            else
                flags = lm == 'h' ? Modifier.h : Modifier.l;
            break;

        default:
            flags = Modifier.none;
            break;
    }

    /* Read the `specifier`
     */
    Format specifier;
    const sc = format[idx];
    genSpecifier = sc;
    switch (sc)
    {
        case 'd':
        case 'i':
            specifier = flags == Modifier.none ? Format.d   :
                        flags == Modifier.hh   ? Format.hhd :
                        flags == Modifier.h    ? Format.hd  :
                        flags == Modifier.ll   ? Format.lld :
                        flags == Modifier.l    ? Format.ld  :
                        flags == Modifier.j    ? Format.jd  :
                        flags == Modifier.z    ? Format.zd  :
                        flags == Modifier.t    ? Format.td  :
                                                 Format.error;
            break;

        case 'u':
        case 'o':
        case 'x':
        case 'X':
            specifier = flags == Modifier.none ? Format.u   :
                        flags == Modifier.hh   ? Format.hhu :
                        flags == Modifier.h    ? Format.hu  :
                        flags == Modifier.ll   ? Format.llu :
                        flags == Modifier.l    ? Format.lu  :
                        flags == Modifier.j    ? Format.ju  :
                        flags == Modifier.z    ? Format.zd  :
                        flags == Modifier.t    ? Format.td  :
                                                 Format.error;
            break;

        case 'f':
        case 'F':
        case 'e':
        case 'E':
        case 'g':
        case 'G':
        case 'a':
        case 'A':
            specifier = flags == Modifier.none ? Format.g  :
                        flags == Modifier.L    ? Format.Lg :
                        flags == Modifier.l    ? Format.lg :
                                                 Format.error;
            break;

        case 'c':
            specifier = flags == Modifier.none ? Format.c       :
                        flags == Modifier.l    ? Format.lc      :
                                                 Format.error;
            break;

        case 's':
            specifier = flags == Modifier.none ? Format.s       :
                        flags == Modifier.l    ? Format.ls      :
                                                 Format.error;
            break;

        case 'p':
            specifier = flags == Modifier.none ? Format.p :
                                                 Format.error;
            break;

        case 'n':
            specifier = flags == Modifier.none ? Format.n   :
                        flags == Modifier.ll   ? Format.lln :
                        flags == Modifier.l    ? Format.ln  :
                        flags == Modifier.hh   ? Format.hhn :
                        flags == Modifier.h    ? Format.hn  :
                        flags == Modifier.j    ? Format.jn  :
                        flags == Modifier.z    ? Format.zn  :
                        flags == Modifier.t    ? Format.tn  :
                                                 Format.error;
            break;

        case 'C':
            // POSIX.1-2017 X/Open System Interfaces (XSI)
            // %C format is equivalent to %lc
            specifier = flags == Modifier.none ? Format.lc :
                                                 Format.error;
            break;

        case 'S':
            // POSIX.1-2017 X/Open System Interfaces (XSI)
            // %S format is equivalent to %ls
            specifier = flags == Modifier.none ? Format.ls :
                                                 Format.error;
            break;

        default:
            specifier = Format.error;
            break;
    }

    ++idx;
    return specifier; // success
}

@("parseGenericFormatSpecifier") unittest
{
    char genSpecifier;
    size_t idx;

    void testG(string fmtStr, Format expectedFormat, char expectedGenSpecifier)
    {
        idx = 0;
        assert(parseGenericFormatSpecifier(fmtStr, idx, genSpecifier) == expectedFormat);
        assert(genSpecifier == expectedGenSpecifier);
    }

    testG("hhd", Format.hhd, 'd');
    testG("hn", Format.hn, 'n');
    testG("ji", Format.jd, 'i');
    testG("lu", Format.lu, 'u');

    idx = 0;
    assert(parseGenericFormatSpecifier("k", idx, genSpecifier) == Format.error);
}

@("parsePrintfFormatSpecifier") unittest
{
    bool useGNUExts = false;

    size_t idx = 0;
    bool widthStar;
    bool precisionStar;

    void testP(string fmtStr, Format expectedFormat, size_t expectedIdx)
    {
        idx = 0;
        assert(parsePrintfFormatSpecifier(fmtStr, idx, widthStar, precisionStar, useGNUExts) == expectedFormat);
        assert(idx == expectedIdx);
    }

    // one for each Format
    testP("%d", Format.d, 2);
    assert(!widthStar && !precisionStar);

    testP("%ld", Format.ld, 3);
    testP("%lld", Format.lld, 4);
    testP("%jd", Format.jd, 3);
    testP("%zd", Format.zd, 3);
    testP("%td", Format.td, 3);
    testP("%g", Format.g, 2);
    testP("%Lg", Format.Lg, 3);
    testP("%p", Format.p, 2);
    testP("%n", Format.n, 2);
    testP("%ln", Format.ln, 3);
    testP("%lln", Format.lln, 4);
    testP("%hn", Format.hn, 3);
    testP("%hhn", Format.hhn, 4);
    testP("%jn", Format.jn, 3);
    testP("%zn", Format.zn, 3);
    testP("%tn", Format.tn, 3);
    testP("%c", Format.c, 2);
    testP("%lc", Format.lc, 3);
    testP("%s", Format.s, 2);
    testP("%ls", Format.ls, 3);
    testP("%%", Format.percent, 2);

    // Synonyms
    testP("%i", Format.d, 2);
    testP("%u", Format.u, 2);
    testP("%o", Format.u, 2);
    testP("%x", Format.u, 2);
    testP("%X", Format.u, 2);
    testP("%f", Format.g, 2);
    testP("%F", Format.g, 2);
    testP("%G", Format.g, 2);
    testP("%a", Format.g, 2);
    testP("%La", Format.Lg, 3);
    testP("%A", Format.g, 2);
    testP("%lg", Format.lg, 3);

    // width, precision
    testP("%*d", Format.d, 3);
    assert(widthStar && !precisionStar);

    testP("%.*d", Format.d, 4);
    assert(!widthStar && precisionStar);

    testP("%*.*d", Format.d, 5);
    assert(widthStar && precisionStar);

    // Too short formats
    foreach (s; ["%", "%-", "%+", "% ", "%#", "%0", "%*", "%1", "%19", "%.", "%.*", "%.1", "%.12",
                    "%j", "%z", "%t", "%l", "%h", "%ll", "%hh"])
    {
        testP(s, Format.error, s.length);
    }

    // Undefined format combinations
    foreach (s; ["%#d", "%llg", "%jg", "%zg", "%tg", "%hg", "%hhg",
                    "%#c", "%0c", "%jc", "%zc", "%tc", "%Lc", "%hc", "%hhc", "%llc",
                    "%#s", "%0s", "%js", "%zs", "%ts", "%Ls", "%hs", "%hhs", "%lls",
                    "%jp", "%zp", "%tp", "%Lp", "%hp", "%lp", "%hhp", "%llp",
                    "%-n", "%+n", "% n", "%#n", "%0n", "%*n", "%1n", "%19n", "%.n", "%.*n", "%.1n", "%.12n", "%Ln", "%K"])
    {
        testP(s, Format.error, s.length);
    }

    testP("%C", Format.lc, 2);
    testP("%S", Format.ls, 2);

    // GNU extensions: explicitly toggle ISO/GNU flag.
    foreach (s; ["%jm", "%zm", "%tm", "%Lm", "%hm", "%hhm", "%lm", "%llm",
                    "%#m", "%+m", "%-m", "% m", "%0m"])
    {
        useGNUExts = false;
        testP(s, Format.error, s.length);
        useGNUExts = true;
        testP(s, Format.error, s.length);
    }

    foreach (s; ["%m", "%md", "%mz", "%mc", "%mm", "%msyz", "%ml", "%mlz", "%mlc", "%mlm"])
    {
        // valid cases, all parsed as `%m`
        // GNU printf()
        useGNUExts = true;
        testP(s, Format.GNU_m, 2);

        // ISO printf()
        useGNUExts = false;
        testP(s, Format.error, 2);
    }
}

@("parseScanfFormatSpecifier") unittest
{
    size_t idx;
    bool asterisk;

    void testS(string fmtStr, Format expectedFormat, size_t expectedIdx)
    {
        idx = 0;
        assert(parseScanfFormatSpecifier(fmtStr, idx, asterisk) == expectedFormat);
        assert(idx == expectedIdx);
    }

    // one for each Format
    testS("%d", Format.d, 2);
    testS("%hhd", Format.hhd, 4);
    testS("%hd", Format.hd, 3);
    testS("%ld", Format.ld, 3);
    testS("%lld", Format.lld, 4);
    testS("%jd", Format.jd, 3);
    testS("%zd", Format.zd, 3);
    testS("%td", Format.td, 3);
    testS("%u", Format.u, 2);
    testS("%hhu", Format.hhu, 4);
    testS("%hu", Format.hu, 3);
    testS("%lu", Format.lu, 3);
    testS("%llu", Format.llu, 4);
    testS("%ju", Format.ju, 3);
    testS("%g", Format.g, 2);
    testS("%lg", Format.lg, 3);
    testS("%Lg", Format.Lg, 3);
    testS("%p", Format.p, 2);
    testS("%s", Format.s, 2);
    testS("%ls", Format.ls, 3);
    testS("%%", Format.percent, 2);

    // Synonyms
    testS("%i", Format.d, 2);
    testS("%n", Format.n, 2);

    testS("%o", Format.u, 2);
    testS("%x", Format.u, 2);
    testS("%f", Format.g, 2);
    testS("%e", Format.g, 2);
    testS("%a", Format.g, 2);
    testS("%c", Format.c, 2);

    // asterisk
    testS("%*d", Format.d, 3);
    assert(asterisk);

    testS("%9ld", Format.ld, 4);
    assert(!asterisk);

    testS("%*25984hhd", Format.hhd, 10);
    assert(asterisk);

    // scansets
    testS("%[a-zA-Z]", Format.s, 9);
    assert(!asterisk);

    testS("%*25l[a-z]", Format.ls, 10);
    assert(asterisk);

    testS("%[]]", Format.s, 4);
    assert(!asterisk);

    testS("%[^]]", Format.s, 5);
    assert(!asterisk);

    // Too short formats
    foreach (s; ["%", "% ", "%#", "%0", "%*", "%1", "%19",
                 "%j", "%z", "%t", "%l", "%h", "%ll", "%hh", "%K"])
    {

        testS(s, Format.error, s.length);
    }


    // Undefined format combinations
    foreach (s; ["%Ld", "%llg", "%jg", "%zg", "%tg", "%hg", "%hhg",
                 "%jc", "%zc", "%tc", "%Lc", "%hc", "%hhc", "%llc",
                 "%jp", "%zp", "%tp", "%Lp", "%hp", "%lp", "%hhp", "%llp",
                 "%-", "%+", "%#", "%0", "%.", "%Ln"])
    {

        testS(s, Format.error, s.length);

    }

    // Invalid scansets
    foreach (s; ["%[]", "%[^", "%[^]", "%[s", "%[0-9lld", "%[", "%l[^]"])
    {

        testS(s, Format.error, s.length);
    }

    // Posix extensions
    foreach (s; ["%jm", "%zm", "%tm", "%Lm", "%hm", "%hhm", "%lm", "%llm",
                 "%m", "%ma", "%md", "%ml", "%mm", "%mlb", "%mlj", "%mlr", "%mlz",
                 "%LC", "%lC", "%llC", "%jC", "%tC", "%hC", "%hhC", "%zC",
                 "%LS", "%lS", "%llS", "%jS", "%tS", "%hS", "%hhS", "%zS"])
    {

        testS(s, Format.error, s.length);
    }

    testS("%mc", Format.POSIX_ms, 3);
    testS("%ms", Format.POSIX_ms, 3);
    testS("%m[0-9]", Format.POSIX_ms, 7);
    testS("%mlc", Format.POSIX_mls, 4);
    testS("%mls", Format.POSIX_mls, 4);
    testS("%ml[^0-9]", Format.POSIX_mls, 9);
    testS("%mC", Format.POSIX_mls, 3);
    testS("%mS", Format.POSIX_mls, 3);

    testS("%C", Format.lc, 2);
    testS("%S", Format.ls, 2);
}
