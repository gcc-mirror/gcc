
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/errors.h
 */

#pragma once

#include "root/dsystem.h"

struct Loc;

// Constants used to discriminate kinds of error messages.
enum class ErrorKind
{
    warning = 0,
    deprecation = 1,
    error = 2,
    tip = 3,
    message = 4,
};

#if defined(__GNUC__)
#define D_ATTRIBUTE_FORMAT(m, n) __attribute__((format(printf, m, n))) __attribute__((nonnull (m)))
#else
#define D_ATTRIBUTE_FORMAT(m, n)
#endif

// Print a warning, deprecation, or error, accepts printf-like format specifiers.
D_ATTRIBUTE_FORMAT(2, 3) void warning(Loc loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void warningSupplemental(Loc loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void deprecation(Loc loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void deprecationSupplemental(Loc loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void error(Loc loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(4, 5) void error(const char *filename, unsigned linnum, unsigned charnum, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void errorSupplemental(Loc loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(1, 2) void message(const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void message(Loc loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(1, 2) void tip(const char *format, ...);

#if defined(__GNUC__) || defined(__clang__)
#define D_ATTRIBUTE_NORETURN __attribute__((noreturn))
#elif _MSC_VER
#define D_ATTRIBUTE_NORETURN __declspec(noreturn)
#else
#define D_ATTRIBUTE_NORETURN
#endif

// Called after printing out fatal error messages.
D_ATTRIBUTE_NORETURN void fatal();
D_ATTRIBUTE_NORETURN void halt();
