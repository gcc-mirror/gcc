/**
 * Check the arguments to `printf` and `scanf` against the `format` string.
 *
 * Copyright:   Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
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
 * http://www.cplusplus.com/reference/cstdio/printf/
 */
bool checkPrintfFormat(ref const Loc loc, scope const char[] format, scope Expression[] args, bool isVa_list)
{
    //printf("checkPrintFormat('%.*s')\n", cast(int)format.length, format.ptr);
    size_t n, gnu_m_count;    // index in args / number of Format.GNU_m
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

        if (isVa_list)
        {
            // format check only
            if (fmt == Format.error)
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", cast(int)slice.length, slice.ptr);
            continue;
        }

        if (fmt == Format.GNU_m)
            ++gnu_m_count;

        Expression getNextArg(ref bool skip)
        {
            if (n == args.length)
            {
                if (args.length < (n + 1) - gnu_m_count)
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

            case Format.GNU_a:  // Format.GNU_a is only for scanf
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
                break;  // not assert(0) because it may go through it if there are extra arguments

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
 * http://www.cplusplus.com/reference/cstdio/scanf/
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

            case Format.GNU_a:
            case Format.GNU_m:
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

            case Format.error:
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", cast(int)slice.length, slice.ptr);
                break;

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

    /* Read the scanset
     * A scanset can be anything, so we just check that it is paired
     */
    if (format[i] == '[')
    {
        while (i < length)
        {
            if (format[i] == ']')
                break;
            ++i;
        }

        // no `]` found
        if (i == length)
            return error();

        ++i;
        // no specifier after `]`
        // it could be mixed with the one above, but then idx won't have the right index
        if (i == length)
            return error();
    }

    /* Read the specifier
     */
    char genSpec;
    Format specifier = parseGenericFormatSpecifier(format, i, genSpec);
    if (specifier == Format.error)
        return error();

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
 * Returns:
 *      Format
 */
Format parsePrintfFormatSpecifier(scope const char[] format, ref size_t idx,
        out bool widthStar, out bool precisionStar) nothrow pure @safe
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
    Format specifier = parseGenericFormatSpecifier(format, i, genSpec);
    if (specifier == Format.error)
        return error();

    switch (genSpec)
    {
        case 'c':
        case 's':
            if (hash || zero)
                return error();
            break;

        case 'd':
        case 'i':
            if (hash)
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
    GNU_a,      // GNU ext. : address to a string with no maximum size (scanf)
    GNU_m,      // GNU ext. : string corresponding to the error code in errno (printf) / length modifier (scanf)
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
    ref size_t idx, out char genSpecifier, bool useGNUExts =
    findCondition(global.versionids, Identifier.idPool("CRuntime_Glibc"))) nothrow pure @trusted
{
    const length = format.length;

    /* Read the `length modifier`
     */
    const lm = format[idx];
    bool lm1;        // if jztL
    bool lm2;        // if `hh` or `ll`
    if (lm == 'j' ||
        lm == 'z' ||
        lm == 't' ||
        lm == 'L')
    {
        ++idx;
        if (idx == length)
            return Format.error;
        lm1 = true;
    }
    else if (lm == 'h' || lm == 'l')
    {
        ++idx;
        if (idx == length)
            return Format.error;
        lm2 = lm == format[idx];
        if (lm2)
        {
            ++idx;
            if (idx == length)
                return Format.error;
        }
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
            if (lm == 'L')
                specifier = Format.error;
            else
                specifier = lm == 'h' && lm2 ? Format.hhd :
                            lm == 'h'        ? Format.hd  :
                            lm == 'l' && lm2 ? Format.lld :
                            lm == 'l'        ? Format.ld  :
                            lm == 'j'        ? Format.jd  :
                            lm == 'z'        ? Format.zd  :
                            lm == 't'        ? Format.td  :
                                               Format.d;
            break;

        case 'u':
        case 'o':
        case 'x':
        case 'X':
            if (lm == 'L')
                specifier = Format.error;
            else
                specifier = lm == 'h' && lm2 ? Format.hhu :
                            lm == 'h'        ? Format.hu  :
                            lm == 'l' && lm2 ? Format.llu :
                            lm == 'l'        ? Format.lu  :
                            lm == 'j'        ? Format.ju  :
                            lm == 'z'        ? Format.zd  :
                            lm == 't'        ? Format.td  :
                                               Format.u;
            break;

        case 'a':
            if (useGNUExts)
            {
                // https://www.gnu.org/software/libc/manual/html_node/Dynamic-String-Input.html
                specifier = Format.GNU_a;
                break;
            }
            goto case;

        case 'f':
        case 'F':
        case 'e':
        case 'E':
        case 'g':
        case 'G':
        case 'A':
            if (lm == 'L')
                specifier = Format.Lg;
            else if (lm1 || lm2 || lm == 'h')
                specifier = Format.error;
            else
                specifier = lm == 'l' ? Format.lg : Format.g;
            break;

        case 'c':
            if (lm1 || lm2 || lm == 'h')
                specifier = Format.error;
            else
                specifier = lm == 'l' ? Format.lc : Format.c;
            break;

        case 's':
            if (lm1 || lm2 || lm == 'h')
                specifier = Format.error;
            else
                specifier = lm == 'l' ? Format.ls : Format.s;
            break;

        case 'p':
            if (lm1 || lm2 || lm == 'h' || lm == 'l')
                specifier = Format.error;
            else
                specifier = Format.p;
            break;

        case 'n':
            if (lm == 'L')
                specifier = Format.error;
            else
                specifier = lm == 'l' && lm2 ? Format.lln :
                            lm == 'l'        ? Format.ln  :
                            lm == 'h' && lm2 ? Format.hhn :
                            lm == 'h'        ? Format.hn  :
                            lm == 'j'        ? Format.jn  :
                            lm == 'z'        ? Format.zn  :
                            lm == 't'        ? Format.tn  :
                                               Format.n;
            break;

        case 'm':
            if (useGNUExts)
            {
                // http://www.gnu.org/software/libc/manual/html_node/Other-Output-Conversions.html
                specifier = Format.GNU_m;
                break;
            }
            goto default;

        default:
            specifier = Format.error;
            break;
    }

    ++idx;
    return specifier; // success
}

unittest
{
    /* parseGenericFormatSpecifier
     */

    char genSpecifier;
    size_t idx;

    assert(parseGenericFormatSpecifier("hhd", idx, genSpecifier) == Format.hhd);
    assert(genSpecifier == 'd');

    idx = 0;
    assert(parseGenericFormatSpecifier("hn", idx, genSpecifier) == Format.hn);
    assert(genSpecifier == 'n');

    idx = 0;
    assert(parseGenericFormatSpecifier("ji", idx, genSpecifier) == Format.jd);
    assert(genSpecifier == 'i');

    idx = 0;
    assert(parseGenericFormatSpecifier("lu", idx, genSpecifier) == Format.lu);
    assert(genSpecifier == 'u');

    idx = 0;
    assert(parseGenericFormatSpecifier("k", idx, genSpecifier) == Format.error);

    /* parsePrintfFormatSpecifier
     */

     bool widthStar;
     bool precisionStar;

     // one for each Format
     idx = 0;
     assert(parsePrintfFormatSpecifier("%d", idx, widthStar, precisionStar) == Format.d);
     assert(idx == 2);
     assert(!widthStar && !precisionStar);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%ld", idx, widthStar, precisionStar) == Format.ld);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%lld", idx, widthStar, precisionStar) == Format.lld);
     assert(idx == 4);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%jd", idx, widthStar, precisionStar) == Format.jd);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%zd", idx, widthStar, precisionStar) == Format.zd);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%td", idx, widthStar, precisionStar) == Format.td);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%g", idx, widthStar, precisionStar) == Format.g);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%Lg", idx, widthStar, precisionStar) == Format.Lg);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%p", idx, widthStar, precisionStar) == Format.p);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%n", idx, widthStar, precisionStar) == Format.n);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%ln", idx, widthStar, precisionStar) == Format.ln);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%lln", idx, widthStar, precisionStar) == Format.lln);
     assert(idx == 4);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%hn", idx, widthStar, precisionStar) == Format.hn);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%hhn", idx, widthStar, precisionStar) == Format.hhn);
     assert(idx == 4);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%jn", idx, widthStar, precisionStar) == Format.jn);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%zn", idx, widthStar, precisionStar) == Format.zn);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%tn", idx, widthStar, precisionStar) == Format.tn);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%c", idx, widthStar, precisionStar) == Format.c);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%lc", idx, widthStar, precisionStar) == Format.lc);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%s", idx, widthStar, precisionStar) == Format.s);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%ls", idx, widthStar, precisionStar) == Format.ls);
     assert(idx == 3);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%%", idx, widthStar, precisionStar) == Format.percent);
     assert(idx == 2);

     // Synonyms
     idx = 0;
     assert(parsePrintfFormatSpecifier("%i", idx, widthStar, precisionStar) == Format.d);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%u", idx, widthStar, precisionStar) == Format.u);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%o", idx, widthStar, precisionStar) == Format.u);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%x", idx, widthStar, precisionStar) == Format.u);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%X", idx, widthStar, precisionStar) == Format.u);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%f", idx, widthStar, precisionStar) == Format.g);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%F", idx, widthStar, precisionStar) == Format.g);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%G", idx, widthStar, precisionStar) == Format.g);
     assert(idx == 2);

     idx = 0;
     Format g = parsePrintfFormatSpecifier("%a", idx, widthStar, precisionStar);
     assert(g == Format.g || g == Format.GNU_a);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%A", idx, widthStar, precisionStar) == Format.g);
     assert(idx == 2);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%lg", idx, widthStar, precisionStar) == Format.lg);
     assert(idx == 3);

     // width, precision
     idx = 0;
     assert(parsePrintfFormatSpecifier("%*d", idx, widthStar, precisionStar) == Format.d);
     assert(idx == 3);
     assert(widthStar && !precisionStar);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%.*d", idx, widthStar, precisionStar) == Format.d);
     assert(idx == 4);
     assert(!widthStar && precisionStar);

     idx = 0;
     assert(parsePrintfFormatSpecifier("%*.*d", idx, widthStar, precisionStar) == Format.d);
     assert(idx == 5);
     assert(widthStar && precisionStar);

     // Too short formats
     {
         foreach (s; ["%", "%-", "%+", "% ", "%#", "%0", "%*", "%1", "%19", "%.", "%.*", "%.1", "%.12",
                      "%j", "%z", "%t", "%l", "%h", "%ll", "%hh"])
         {
             idx = 0;
             assert(parsePrintfFormatSpecifier(s, idx, widthStar, precisionStar) == Format.error);
             assert(idx == s.length);
         }
     }

     // Undefined format combinations
     {
         foreach (s; ["%#d", "%llg", "%jg", "%zg", "%tg", "%hg", "%hhg",
                      "%#c", "%0c", "%jc", "%zc", "%tc", "%Lc", "%hc", "%hhc", "%llc",
                      "%#s", "%0s", "%js", "%zs", "%ts", "%Ls", "%hs", "%hhs", "%lls",
                      "%jp", "%zp", "%tp", "%Lp", "%hp", "%lp", "%hhp", "%llp",
                      "%-n", "%+n", "% n", "%#n", "%0n", "%*n", "%1n", "%19n", "%.n", "%.*n", "%.1n", "%.12n", "%Ln", "%K"])
         {
             idx = 0;
             assert(parsePrintfFormatSpecifier(s, idx, widthStar, precisionStar) == Format.error);
             assert(idx == s.length);
         }
     }

    /* parseScanfFormatSpecifier
     */

    bool asterisk;

    // one for each Format
    idx = 0;
    assert(parseScanfFormatSpecifier("%d", idx, asterisk) == Format.d);
    assert(idx == 2);
    assert(!asterisk);

    idx = 0;
    assert(parseScanfFormatSpecifier("%hhd", idx, asterisk) == Format.hhd);
    assert(idx == 4);

    idx = 0;
    assert(parseScanfFormatSpecifier("%hd", idx, asterisk) == Format.hd);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%ld", idx, asterisk) == Format.ld);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%lld", idx, asterisk) == Format.lld);
    assert(idx == 4);

    idx = 0;
    assert(parseScanfFormatSpecifier("%jd", idx, asterisk) == Format.jd);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%zd", idx, asterisk) == Format.zd);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%td", idx, asterisk,) == Format.td);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%u", idx, asterisk) == Format.u);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%hhu", idx, asterisk,) == Format.hhu);
    assert(idx == 4);

    idx = 0;
    assert(parseScanfFormatSpecifier("%hu", idx, asterisk) == Format.hu);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%lu", idx, asterisk) == Format.lu);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%llu", idx, asterisk) == Format.llu);
    assert(idx == 4);

    idx = 0;
    assert(parseScanfFormatSpecifier("%ju", idx, asterisk) == Format.ju);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%g", idx, asterisk) == Format.g);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%lg", idx, asterisk) == Format.lg);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%Lg", idx, asterisk) == Format.Lg);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%p", idx, asterisk) == Format.p);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%s", idx, asterisk) == Format.s);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%ls", idx, asterisk,) == Format.ls);
    assert(idx == 3);

    idx = 0;
    assert(parseScanfFormatSpecifier("%%", idx, asterisk) == Format.percent);
    assert(idx == 2);

    // Synonyms
    idx = 0;
    assert(parseScanfFormatSpecifier("%i", idx, asterisk) == Format.d);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%n", idx, asterisk) == Format.n);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%o", idx, asterisk) == Format.u);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%x", idx, asterisk) == Format.u);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%f", idx, asterisk) == Format.g);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%e", idx, asterisk) == Format.g);
    assert(idx == 2);

    idx = 0;
    g = parseScanfFormatSpecifier("%a", idx, asterisk);
    assert(g == Format.g || g == Format.GNU_a);
    assert(idx == 2);

    idx = 0;
    assert(parseScanfFormatSpecifier("%c", idx, asterisk) == Format.c);
    assert(idx == 2);

    // asterisk
    idx = 0;
    assert(parseScanfFormatSpecifier("%*d", idx, asterisk) == Format.d);
    assert(idx == 3);
    assert(asterisk);

    idx = 0;
    assert(parseScanfFormatSpecifier("%9ld", idx, asterisk) == Format.ld);
    assert(idx == 4);
    assert(!asterisk);

    idx = 0;
    assert(parseScanfFormatSpecifier("%*25984hhd", idx, asterisk) == Format.hhd);
    assert(idx == 10);
    assert(asterisk);

    // scansets
    idx = 0;
    assert(parseScanfFormatSpecifier("%[a-zA-Z]s", idx, asterisk) == Format.s);
    assert(idx == 10);
    assert(!asterisk);

    idx = 0;
    assert(parseScanfFormatSpecifier("%*25[a-z]hhd", idx, asterisk) == Format.hhd);
    assert(idx == 12);
    assert(asterisk);

    // Too short formats
    foreach (s; ["%", "% ", "%#", "%0", "%*", "%1", "%19",
                 "%j", "%z", "%t", "%l", "%h", "%ll", "%hh", "%K"])
    {
        idx = 0;
        assert(parseScanfFormatSpecifier(s, idx, asterisk) == Format.error);
        assert(idx == s.length);
    }


    // Undefined format combinations
    foreach (s; ["%Ld", "%llg", "%jg", "%zg", "%tg", "%hg", "%hhg",
                 "%jc", "%zc", "%tc", "%Lc", "%hc", "%hhc", "%llc",
                 "%jp", "%zp", "%tp", "%Lp", "%hp", "%lp", "%hhp", "%llp",
                 "%-", "%+", "%#", "%0", "%.", "%Ln"])
    {
        idx = 0;
        assert(parseScanfFormatSpecifier(s, idx, asterisk) == Format.error);
        assert(idx == s.length);

    }

    // Invalid scansets
    foreach (s; ["%[]", "%[s", "%[0-9lld", "%[", "%[a-z]"])
    {
        idx = 0;
        assert(parseScanfFormatSpecifier(s, idx, asterisk) == Format.error);
        assert(idx == s.length);
    }

}
