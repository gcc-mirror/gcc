
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/errors.h
 */

#pragma once

#include "root/dsystem.h"
#include "globals.h"

bool isConsoleColorSupported();

#if defined(__GNUC__)
#define D_ATTRIBUTE_FORMAT(m, n) __attribute__((format(printf, m, n))) __attribute__((nonnull (m)))
#else
#define D_ATTRIBUTE_FORMAT(m, n)
#endif

// Print a warning, deprecation, or error, accepts printf-like format specifiers.
D_ATTRIBUTE_FORMAT(2, 3) void warning(const Loc& loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void warningSupplemental(const Loc& loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void deprecation(const Loc& loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void deprecationSupplemental(const Loc& loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void error(const Loc& loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void errorSupplemental(const Loc& loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 0) void verror(const Loc& loc, const char *format, va_list ap, const char *p1 = NULL, const char *p2 = NULL, const char *header = "Error: ");
D_ATTRIBUTE_FORMAT(2, 0) void verrorSupplemental(const Loc& loc, const char *format, va_list ap);
D_ATTRIBUTE_FORMAT(2, 0) void vwarning(const Loc& loc, const char *format, va_list);
D_ATTRIBUTE_FORMAT(2, 0) void vwarningSupplemental(const Loc& loc, const char *format, va_list ap);
D_ATTRIBUTE_FORMAT(2, 0) void vdeprecation(const Loc& loc, const char *format, va_list ap, const char *p1 = NULL, const char *p2 = NULL);
D_ATTRIBUTE_FORMAT(2, 0) void vdeprecationSupplemental(const Loc& loc, const char *format, va_list ap);
D_ATTRIBUTE_FORMAT(1, 2) void message(const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 3) void message(const Loc& loc, const char *format, ...);
D_ATTRIBUTE_FORMAT(2, 0) void vmessage(const Loc& loc, const char *format, va_list);

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
