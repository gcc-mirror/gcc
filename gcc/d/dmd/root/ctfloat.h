
/* Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/ctfloat.h
 */

#pragma once

#include "dcompat.h"
#include "longdouble.h"

// Type used by the front-end for compile-time reals
typedef longdouble real_t;

// Compile-time floating-point helper
struct CTFloat
{
    static void yl2x(const real_t *x, const real_t *y, real_t *res);
    static void yl2xp1(const real_t *x, const real_t *y, real_t *res);

    static real_t sin(real_t x);
    static real_t cos(real_t x);
    static real_t tan(real_t x);
    static real_t sqrt(real_t x);
    static real_t fabs(real_t x);
    static real_t ldexp(real_t n, int exp);

    static real_t round(real_t x);
    static real_t floor(real_t x);
    static real_t ceil(real_t x);
    static real_t trunc(real_t x);
    static real_t log(real_t x);
    static real_t log2(real_t x);
    static real_t log10(real_t x);
    static real_t pow(real_t x, real_t y);
    static real_t exp(real_t x);
    static real_t expm1(real_t x);
    static real_t exp2(real_t x);

    static real_t fmin(real_t x, real_t y);
    static real_t fmax(real_t x, real_t y);
    static real_t copysign(real_t x, real_t s);

    static real_t fma(real_t x, real_t y, real_t z);

    static bool isIdentical(real_t a, real_t b);
    static bool isNaN(real_t r);
    static bool isSNaN(real_t r);
    static bool isInfinity(real_t r);

    static real_t parse(const char *literal, bool& isOutOfRange);
    static int sprint(char *str, d_size_t size, char fmt, real_t x);

    static size_t hash(real_t a);

    // Constant real values 0, 1, -1 and 0.5.
    static real_t zero;
    static real_t one;
    static real_t minusone;
    static real_t half;

    static void initialize();
};
