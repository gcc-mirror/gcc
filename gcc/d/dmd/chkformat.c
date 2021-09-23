
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

// Check the arguments to `printf` and `scanf` against the `format` string.

#include "root/dsystem.h"
#include "root/dcompat.h"

#include "arraytypes.h"
#include "cond.h"
#include "errors.h"
#include "expression.h"
#include "globals.h"
#include "identifier.h"
#include "mtype.h"
#include "target.h"


/* Different kinds of formatting specifications, variations we don't
   care about are merged. (Like we don't care about the difference between
   f, e, g, a, etc.)

   For `scanf`, every format is a pointer.
 */
enum Format
{
    Format_d,          // int
    Format_hhd,        // signed char
    Format_hd,         // short int
    Format_ld,         // long int
    Format_lld,        // long long int
    Format_jd,         // intmax_t
    Format_zd,         // size_t
    Format_td,         // ptrdiff_t
    Format_u,          // unsigned int
    Format_hhu,        // unsigned char
    Format_hu,         // unsigned short int
    Format_lu,         // unsigned long int
    Format_llu,        // unsigned long long int
    Format_ju,         // uintmax_t
    Format_g,          // float (scanf) / double (printf)
    Format_lg,         // double (scanf)
    Format_Lg,         // long double (both)
    Format_s,          // char string (both)
    Format_ls,         // wchar_t string (both)
    Format_c,          // char (printf)
    Format_lc,         // wint_t (printf)
    Format_p,          // pointer
    Format_n,          // pointer to int
    Format_hhn,        // pointer to signed char
    Format_hn,         // pointer to short
    Format_ln,         // pointer to long int
    Format_lln,        // pointer to long long int
    Format_jn,         // pointer to intmax_t
    Format_zn,         // pointer to size_t
    Format_tn,         // pointer to ptrdiff_t
    Format_GNU_a,      // GNU ext. : address to a string with no maximum size (scanf)
    Format_GNU_m,      // GNU ext. : string corresponding to the error code in errno (printf) / length modifier (scanf)
    Format_percent,    // %% (i.e. no argument)
    Format_error,      // invalid format specification
};

/**************************************
 * Parse the *length specifier* and the *specifier* of the following form:
 * `[length]specifier`
 *
 * Params:
 *      format = format string
 *      idx = index of of start of format specifier,
 *          which gets updated to index past the end of it,
 *          even if `Format_error` is returned
 *      genSpecifier = Generic specifier. For instance, it will be set to `d` if the
 *           format is `hdd`.
 * Returns:
 *      Format
 */
static Format parseGenericFormatSpecifier(const char *format,
            size_t &idx, char &genSpecifier, bool useGNUExts =
            findCondition(global.versionids, Identifier::idPool("CRuntime_Glibc")))
{
    genSpecifier = 0;

    const size_t length = strlen(format);

    /* Read the `length modifier`
     */
    const char lm = format[idx];
    bool lm1= false;        // if jztL
    bool lm2= false;        // if `hh` or `ll`
    if (lm == 'j' ||
        lm == 'z' ||
        lm == 't' ||
        lm == 'L')
    {
        ++idx;
        if (idx == length)
            return Format_error;
        lm1 = true;
    }
    else if (lm == 'h' || lm == 'l')
    {
        ++idx;
        if (idx == length)
            return Format_error;
        lm2 = lm == format[idx];
        if (lm2)
        {
            ++idx;
            if (idx == length)
                return Format_error;
        }
    }

    /* Read the `specifier`
     */
    Format specifier;
    const char sc = format[idx];
    genSpecifier = sc;
    switch (sc)
    {
        case 'd':
        case 'i':
            if (lm == 'L')
                specifier = Format_error;
            else
                specifier = lm == 'h' && lm2 ? Format_hhd :
                            lm == 'h'        ? Format_hd  :
                            lm == 'l' && lm2 ? Format_lld :
                            lm == 'l'        ? Format_ld  :
                            lm == 'j'        ? Format_jd  :
                            lm == 'z'        ? Format_zd  :
                            lm == 't'        ? Format_td  :
                                               Format_d;
            break;

        case 'u':
        case 'o':
        case 'x':
        case 'X':
            if (lm == 'L')
                specifier = Format_error;
            else
                specifier = lm == 'h' && lm2 ? Format_hhu :
                            lm == 'h'        ? Format_hu  :
                            lm == 'l' && lm2 ? Format_llu :
                            lm == 'l'        ? Format_lu  :
                            lm == 'j'        ? Format_ju  :
                            lm == 'z'        ? Format_zd  :
                            lm == 't'        ? Format_td  :
                                               Format_u;
            break;

        case 'a':
            if (useGNUExts)
            {
                // https://www.gnu.org/software/libc/manual/html_node/Dynamic-String-Input.html
                specifier = Format_GNU_a;
                break;
            }
            /* fall through */

        case 'f':
        case 'F':
        case 'e':
        case 'E':
        case 'g':
        case 'G':
        case 'A':
            if (lm == 'L')
                specifier = Format_Lg;
            else if (lm1 || lm2 || lm == 'h')
                specifier = Format_error;
            else
                specifier = lm == 'l' ? Format_lg : Format_g;
            break;

        case 'c':
            if (lm1 || lm2 || lm == 'h')
                specifier = Format_error;
            else
                specifier = lm == 'l' ? Format_lc : Format_c;
            break;

        case 's':
            if (lm1 || lm2 || lm == 'h')
                specifier = Format_error;
            else
                specifier = lm == 'l' ? Format_ls : Format_s;
            break;

        case 'p':
            if (lm1 || lm2 || lm == 'h' || lm == 'l')
                specifier = Format_error;
            else
                specifier = Format_p;
            break;

        case 'n':
            if (lm == 'L')
                specifier = Format_error;
            else
                specifier = lm == 'l' && lm2 ? Format_lln :
                            lm == 'l'        ? Format_ln  :
                            lm == 'h' && lm2 ? Format_hhn :
                            lm == 'h'        ? Format_hn  :
                            lm == 'j'        ? Format_jn  :
                            lm == 'z'        ? Format_zn  :
                            lm == 't'        ? Format_tn  :
                                               Format_n;
            break;

        case 'm':
            if (useGNUExts)
            {
                // http://www.gnu.org/software/libc/manual/html_node/Other-Output-Conversions.html
                specifier = Format_GNU_m;
                break;
            }
            goto Ldefault;

        default:
        Ldefault:
            specifier = Format_error;
            break;
    }

    ++idx;
    return specifier; // success
}

Format formatError(size_t &idx, size_t i)
{
    idx = i;
    return Format_error;
}

/**************************************
 * Parse the *format specifier* which is of the form:
 *
 * `%[*][width][length]specifier`
 *
 * Params:
 *      format = format string
 *      idx = index of `%` of start of format specifier,
 *          which gets updated to index past the end of it,
 *          even if `Format_error` is returned
 *      asterisk = set if there is a `*` sub-specifier
 * Returns:
 *      Format
 */
static Format parseScanfFormatSpecifier(const char *format, size_t &idx,
                bool &asterisk)
{
    asterisk = false;

    size_t i = idx;
    assert(format[i] == '%');
    const size_t length = strlen(format);

    ++i;
    if (i == length)
        return formatError(idx, i);

    if (format[i] == '%')
    {
        idx = i + 1;
        return Format_percent;
    }

    // * sub-specifier
    if (format[i] == '*')
    {
        ++i;
        if (i == length)
            return formatError(idx, i);
        asterisk = true;
    }

    // fieldWidth
    while (isdigit(format[i]))
    {
        i++;
        if (i == length)
            return formatError(idx, i);
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
            return formatError(idx, i);

        ++i;
        // no specifier after `]`
        // it could be mixed with the one above, but then idx won't have the right index
        if (i == length)
            return formatError(idx, i);
    }

    /* Read the specifier
     */
    char genSpec;
    Format specifier = parseGenericFormatSpecifier(format, i, genSpec);
    if (specifier == Format_error)
        return formatError(idx, i);

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
 *          even if `Format_error` is returned
 *      widthStar = set if * for width
 *      precisionStar = set if * for precision
 * Returns:
 *      Format
 */
static Format parsePrintfFormatSpecifier(const char *format, size_t &idx,
                bool &widthStar, bool &precisionStar)
{
    widthStar = false;
    precisionStar = false;

    size_t i = idx;
    assert(format[i] == '%');
    const size_t format_length = strlen(format);
    const size_t length = format_length;
    bool hash = false;
    bool zero = false;
    bool flags = false;
    bool width = false;
    bool precision = false;

    ++i;
    if (i == length)
        return formatError(idx, i);

    if (format[i] == '%')
    {
        idx = i + 1;
        return Format_percent;
    }

    /* Read the `flags`
     */
    while (1)
    {
        const char c = format[i];
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
            return formatError(idx, i);
    }

    /* Read the `field width`
     */
    {
        const char c = format[i];
        if (c == '*')
        {
            width = true;
            widthStar = true;
            ++i;
            if (i == length)
                return formatError(idx, i);
        }
        else if ('1' <= c && c <= '9')
        {
            width = true;
            ++i;
            if (i == length)
                return formatError(idx, i);
            while ('0' <= format[i] && format[i] <= '9')
            {
               ++i;
               if (i == length)
                    return formatError(idx, i);
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
            return formatError(idx, i);
        const char c = format[i];
        if (c == '*')
        {
            precisionStar = true;
            ++i;
            if (i == length)
                return formatError(idx, i);
        }
        else if ('0' <= c && c <= '9')
        {
            ++i;
            if (i == length)
                return formatError(idx, i);
            while ('0' <= format[i] && format[i] <= '9')
            {
               ++i;
               if (i == length)
                    return formatError(idx, i);
            }
        }
    }

    /* Read the specifier
     */
    char genSpec;
    Format specifier = parseGenericFormatSpecifier(format, i, genSpec);
    if (specifier == Format_error)
        return formatError(idx, i);

    switch (genSpec)
    {
        case 'c':
        case 's':
            if (hash || zero)
                return formatError(idx, i);
            break;

        case 'd':
        case 'i':
            if (hash)
                return formatError(idx, i);
            break;

        case 'n':
            if (hash || zero || precision || width || flags)
                return formatError(idx, i);
            break;

        default:
            break;
    }

    idx = i;
    return specifier;  // success
}

/*******************************************/

static Expression *getNextPrintfArg(const Loc &loc, Expressions &args, size_t &n,
                size_t gnu_m_count, bool &skip)
{
    if (n == args.length)
    {
        if (args.length < (n + 1) - gnu_m_count)
            deprecation(loc, "more format specifiers than %d arguments", (int)n);
        else
            skip = true;
        return NULL;
    }
    return args[n++];
}

static void errorPrintfFormat(const char *prefix, DString &slice, Expression *arg,
                const char *texpect, Type *tactual)
{
    deprecation(arg->loc, "%sargument `%s` for format specification `\"%.*s\"` must be `%s`, not `%s`",
                prefix ? prefix : "", arg->toChars(), (int)slice.length, slice.ptr, texpect, tactual->toChars());
}

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
bool checkPrintfFormat(const Loc &loc, const char *format, Expressions &args, bool isVa_list)
{
    //printf("checkPrintFormat('%s')\n", format);
    size_t n = 0;             // index in args
    size_t gnu_m_count = 0;   // number of Format_GNU_m
    const size_t format_length = strlen(format);
    for (size_t i = 0; i < format_length;)
    {
        if (format[i] != '%')
        {
            ++i;
            continue;
        }
        bool widthStar = false;
        bool precisionStar = false;
        size_t j = i;
        const Format fmt = parsePrintfFormatSpecifier(format, j, widthStar, precisionStar);
        DString slice = DString(j - i, format + i);
        i = j;

        if (fmt == Format_percent)
            continue;                   // "%%", no arguments

        if (isVa_list)
        {
            // format check only
            if (fmt == Format_error)
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", (int)slice.length, slice.ptr);
            continue;
        }

        if (fmt == Format_GNU_m)
            ++gnu_m_count;

        if (widthStar)
        {
            bool skip = false;
            Expression *e = getNextPrintfArg(loc, args, n, gnu_m_count, skip);
            if (skip)
                continue;
            if (!e)
                return true;
            Type *t = e->type->toBasetype();
            if (t->ty != Tint32 && t->ty != Tuns32)
                errorPrintfFormat("width ", slice, e, "int", t);
        }

        if (precisionStar)
        {
            bool skip = false;
            Expression *e = getNextPrintfArg(loc, args, n, gnu_m_count, skip);
            if (skip)
                continue;
            if (!e)
                return true;
            Type *t = e->type->toBasetype();
            if (t->ty != Tint32 && t->ty != Tuns32)
                errorPrintfFormat("precision ", slice, e, "int", t);
        }

        bool skip = false;
        Expression *e = getNextPrintfArg(loc, args, n, gnu_m_count, skip);
        if (skip)
            continue;
        if (!e)
            return true;
        Type *t = e->type->toBasetype();
        Type *tnext = t->nextOf();
        const unsigned c_longsize = target.c.longsize;
        const unsigned ptrsize = target.ptrsize;

        // Types which are promoted to int are allowed.
        // Spec: C99 6.5.2.2.7
        switch (fmt)
        {
            case Format_u:      // unsigned int
            case Format_d:      // int
                if (t->ty != Tint32 && t->ty != Tuns32)
                    errorPrintfFormat(NULL, slice, e, fmt == Format_u ? "uint" : "int", t);
                break;

            case Format_hhu:    // unsigned char
            case Format_hhd:    // signed char
                if (t->ty != Tint32 && t->ty != Tuns32 && t->ty != Tint8 && t->ty != Tuns8)
                    errorPrintfFormat(NULL, slice, e, fmt == Format_hhu ? "ubyte" : "byte", t);
                break;

            case Format_hu:     // unsigned short int
            case Format_hd:     // short int
                if (t->ty != Tint32 && t->ty != Tuns32 && t->ty != Tint16 && t->ty != Tuns16)
                    errorPrintfFormat(NULL, slice, e, fmt == Format_hu ? "ushort" : "short", t);
                break;

            case Format_lu:     // unsigned long int
            case Format_ld:     // long int
                if (!(t->isintegral() && t->size() == c_longsize))
                {
                    if (fmt == Format_lu)
                        errorPrintfFormat(NULL, slice, e, (c_longsize == 4 ? "uint" : "ulong"), t);
                    else
                        errorPrintfFormat(NULL, slice, e, (c_longsize == 4 ? "int" : "long"), t);
                }
                break;

            case Format_llu:    // unsigned long long int
            case Format_lld:    // long long int
                if (t->ty != Tint64 && t->ty != Tuns64)
                    errorPrintfFormat(NULL, slice, e, fmt == Format_llu ? "ulong" : "long", t);
                break;

            case Format_ju:     // uintmax_t
            case Format_jd:     // intmax_t
                if (t->ty != Tint64 && t->ty != Tuns64)
                {
                    if (fmt == Format_ju)
                        errorPrintfFormat(NULL, slice, e, "core.stdc.stdint.uintmax_t", t);
                    else
                        errorPrintfFormat(NULL, slice, e, "core.stdc.stdint.intmax_t", t);
                }
                break;

            case Format_zd:     // size_t
                if (!(t->isintegral() && t->size() == ptrsize))
                    errorPrintfFormat(NULL, slice, e, "size_t", t);
                break;

            case Format_td:     // ptrdiff_t
                if (!(t->isintegral() && t->size() == ptrsize))
                    errorPrintfFormat(NULL, slice, e, "ptrdiff_t", t);
                break;

            case Format_GNU_a:  // Format_GNU_a is only for scanf
            case Format_lg:
            case Format_g:      // double
                if (t->ty != Tfloat64 && t->ty != Timaginary64)
                    errorPrintfFormat(NULL, slice, e, "double", t);
                break;

            case Format_Lg:     // long double
                if (t->ty != Tfloat80 && t->ty != Timaginary80)
                    errorPrintfFormat(NULL, slice, e, "real", t);
                break;

            case Format_p:      // pointer
                if (t->ty != Tpointer && t->ty != Tnull && t->ty != Tclass && t->ty != Tdelegate && t->ty != Taarray)
                    errorPrintfFormat(NULL, slice, e, "void*", t);
                break;

            case Format_n:      // pointer to int
                if (!(t->ty == Tpointer && tnext->ty == Tint32))
                    errorPrintfFormat(NULL, slice, e, "int*", t);
                break;

            case Format_ln:     // pointer to long int
                if (!(t->ty == Tpointer && tnext->isintegral() && !tnext->isunsigned() && tnext->size() == c_longsize))
                    errorPrintfFormat(NULL, slice, e, (c_longsize == 4 ? "int*" : "long*"), t);
                break;

            case Format_lln:    // pointer to long long int
                if (!(t->ty == Tpointer && tnext->ty == Tint64))
                    errorPrintfFormat(NULL, slice, e, "long*", t);
                break;

            case Format_hn:     // pointer to short
                if (!(t->ty == Tpointer && tnext->ty == Tint16))
                    errorPrintfFormat(NULL, slice, e, "short*", t);
                break;

            case Format_hhn:    // pointer to signed char
                if (!(t->ty == Tpointer && tnext->ty == Tint16))
                    errorPrintfFormat(NULL, slice, e, "byte*", t);
                break;

            case Format_jn:     // pointer to intmax_t
                if (!(t->ty == Tpointer && tnext->ty == Tint64))
                    errorPrintfFormat(NULL, slice, e, "core.stdc.stdint.intmax_t*", t);
                break;

            case Format_zn:     // pointer to size_t
                if (!(t->ty == Tpointer && tnext->isintegral() && tnext->isunsigned() && tnext->size() == ptrsize))
                    errorPrintfFormat(NULL, slice, e, "size_t*", t);
                break;

            case Format_tn:     // pointer to ptrdiff_t
                if (!(t->ty == Tpointer && tnext->isintegral() && !tnext->isunsigned() && tnext->size() == ptrsize))
                    errorPrintfFormat(NULL, slice, e, "ptrdiff_t*", t);
                break;

            case Format_c:      // char
                if (t->ty != Tint32 && t->ty != Tuns32)
                    errorPrintfFormat(NULL, slice, e, "char", t);
                break;

            case Format_lc:     // wint_t
                if (t->ty != Tint32 && t->ty != Tuns32)
                    errorPrintfFormat(NULL, slice, e, "wchar_t", t);
                break;

            case Format_s:      // pointer to char string
                if (!(t->ty == Tpointer && (tnext->ty == Tchar || tnext->ty == Tint8 || tnext->ty == Tuns8)))
                    errorPrintfFormat(NULL, slice, e, "char*", t);
                break;

            case Format_ls:     // pointer to wchar_t string
            {
                if (!(t->ty == Tpointer && tnext == target.c.twchar_t))
                    errorPrintfFormat(NULL, slice, e, "wchar_t*", t);
                break;
            }
            case Format_error:
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", (int)slice.length, slice.ptr);
                break;

            case Format_GNU_m:
                break;  // not assert(0) because it may go through it if there are extra arguments

            case Format_percent:
            default:
                assert(0);
        }
    }
    return false;
}

/*******************************************/

static Expression *getNextScanfArg(const Loc &loc, Expressions &args, size_t &n, bool asterisk)
{
    if (n == args.length)
    {
        if (!asterisk)
            deprecation(loc, "more format specifiers than %d arguments", (int)n);
        return NULL;
    }
    return args[n++];
}

static void errorScanfFormat(const char *prefix, DString &slice,
                Expression *arg, const char *texpect, Type *tactual)
{
    deprecation(arg->loc, "%sargument `%s` for format specification `\"%.*s\"` must be `%s`, not `%s`",
                prefix ? prefix : "", arg->toChars(), (int)slice.length, slice.ptr, texpect, tactual->toChars());
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
bool checkScanfFormat(const Loc &loc, const char *format, Expressions &args, bool isVa_list)
{
    size_t n = 0;
    const size_t format_length = strlen(format);
    for (size_t i = 0; i < format_length;)
    {
        if (format[i] != '%')
        {
            ++i;
            continue;
        }
        bool asterisk = false;
        size_t j = i;
        const Format fmt = parseScanfFormatSpecifier(format, j, asterisk);
        DString slice = DString(j - i, format + i);
        i = j;

        if (fmt == Format_percent || asterisk)
            continue;   // "%%", "%*": no arguments

        if (isVa_list)
        {
            // format check only
            if (fmt == Format_error)
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", (int)slice.length, slice.ptr);
            continue;
        }

        Expression *e = getNextScanfArg(loc, args, n, asterisk);
        if (!e)
            return true;

        Type *t = e->type->toBasetype();
        Type *tnext = t->nextOf();
        const unsigned c_longsize = target.c.longsize;
        const unsigned ptrsize = target.ptrsize;

        switch (fmt)
        {
            case Format_n:
            case Format_d:      // pointer to int
                if (!(t->ty == Tpointer && tnext->ty == Tint32))
                    errorScanfFormat(NULL, slice, e, "int*", t);
                break;

            case Format_hhn:
            case Format_hhd:    // pointer to signed char
                if (!(t->ty == Tpointer && tnext->ty == Tint16))
                    errorScanfFormat(NULL, slice, e, "byte*", t);
                break;

            case Format_hn:
            case Format_hd:     // pointer to short
                if (!(t->ty == Tpointer && tnext->ty == Tint16))
                    errorScanfFormat(NULL, slice, e, "short*", t);
                break;

            case Format_ln:
            case Format_ld:     // pointer to long int
                if (!(t->ty == Tpointer && tnext->isintegral() && tnext->size() == c_longsize))
                    errorScanfFormat(NULL, slice, e, (c_longsize == 4 ? "int*" : "long*"), t);
                break;

            case Format_lln:
            case Format_lld:    // pointer to long long int
                if (!(t->ty == Tpointer && tnext->ty == Tint64))
                    errorScanfFormat(NULL, slice, e, "long*", t);
                break;

            case Format_jn:
            case Format_jd:     // pointer to intmax_t
                if (!(t->ty == Tpointer && tnext->ty == Tint64))
                    errorScanfFormat(NULL, slice, e, "core.stdc.stdint.intmax_t*", t);
                break;

            case Format_zn:
            case Format_zd:     // pointer to size_t
                if (!(t->ty == Tpointer && tnext->isintegral() && tnext->isunsigned() && tnext->size() == ptrsize))
                    errorScanfFormat(NULL, slice, e, "size_t*", t);
                break;

            case Format_tn:
            case Format_td:     // pointer to ptrdiff_t
                if (!(t->ty == Tpointer && tnext->isintegral() && !tnext->isunsigned() && tnext->size() == ptrsize))
                    errorScanfFormat(NULL, slice, e, "ptrdiff_t*", t);
                break;

            case Format_u:      // pointer to unsigned int
                if (!(t->ty == Tpointer && tnext->ty == Tuns32))
                    errorScanfFormat(NULL, slice, e, "uint*", t);
                break;

            case Format_hhu:    // pointer to unsigned char
                if (!(t->ty == Tpointer && tnext->ty == Tuns8))
                    errorScanfFormat(NULL, slice, e, "ubyte*", t);
                break;

            case Format_hu:     // pointer to unsigned short int
                if (!(t->ty == Tpointer && tnext->ty == Tuns16))
                    errorScanfFormat(NULL, slice, e, "ushort*", t);
                break;

            case Format_lu:     // pointer to unsigned long int
                if (!(t->ty == Tpointer && tnext->isintegral() && tnext->isunsigned() && tnext->size() == c_longsize))
                    errorScanfFormat(NULL, slice, e, (c_longsize == 4 ? "uint*" : "ulong*"), t);
                break;

            case Format_llu:    // pointer to unsigned long long int
                if (!(t->ty == Tpointer && tnext->ty == Tuns64))
                    errorScanfFormat(NULL, slice, e, "ulong*", t);
                break;

            case Format_ju:     // pointer to uintmax_t
                if (!(t->ty == Tpointer && tnext->ty == Tuns64))
                    errorScanfFormat(NULL, slice, e, "ulong*", t);
                break;

            case Format_g:      // pointer to float
                if (!(t->ty == Tpointer && tnext->ty == Tfloat32))
                    errorScanfFormat(NULL, slice, e, "float*", t);
                break;

            case Format_lg:     // pointer to double
                if (!(t->ty == Tpointer && tnext->ty == Tfloat64))
                    errorScanfFormat(NULL, slice, e, "double*", t);
                break;

            case Format_Lg:     // pointer to long double
                if (!(t->ty == Tpointer && tnext->ty == Tfloat80))
                    errorScanfFormat(NULL, slice, e, "real*", t);
                break;

            case Format_GNU_a:
            case Format_GNU_m:
            case Format_c:
            case Format_s:      // pointer to char string
                if (!(t->ty == Tpointer && (tnext->ty == Tchar || tnext->ty == Tint8 || tnext->ty == Tuns8)))
                    errorScanfFormat(NULL, slice, e, "char*", t);
                break;

            case Format_lc:
            case Format_ls:     // pointer to wchar_t string
            {
                if (!(t->ty == Tpointer && tnext == target.c.twchar_t))
                    errorScanfFormat(NULL, slice, e, "wchar_t*", t);
                break;
            }
            case Format_p:      // double pointer
                if (!(t->ty == Tpointer && tnext->ty == Tpointer))
                    errorScanfFormat(NULL, slice, e, "void**", t);
                break;

            case Format_error:
                deprecation(loc, "format specifier `\"%.*s\"` is invalid", (int)slice.length, slice.ptr);
                break;

            case Format_percent:
            default:
                assert(0);
        }
    }
    return false;
}
