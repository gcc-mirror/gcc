/* PR ipa/80732 */
/* { dg-do run } */
/* { dg-options "-ldl -fPIC -rdynamic -O3 -g -pie" } */
/* { dg-require-ifunc "" } */
/* { dg-require-effective-target fma4 } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target pie } */

#include "fma4-check.h"

#include <dlfcn.h>

__attribute__((target_clones("default","fma"),noinline,optimize("fast-math")))
double f1(double a, double b, double c)
{
    return a * b + c;
}

double k1(double a, double b, double c, void **p)
{
    *p = f1;
    return f1(a, b, c);
}

__attribute__((target("fma"),optimize("fast-math")))
static double f2_fma(double a, double b, double c)
{
    return a * b + c;
}

__attribute__((optimize("fast-math")))
static double f2_default(double a, double b, double c)
{
    return a * b + c;
}

static __typeof__ (f2_fma)* f2_resolve(void)
{
    __builtin_cpu_init ();
    if (__builtin_cpu_supports("fma"))
        return f2_fma;
    else
        return f2_default;
}

double f2(double a, double b, double c) __attribute__((ifunc("f2_resolve")));

double k2(double a, double b, double c, void **p)
{
    *p = f2;
    return f2(a, b, c);
}

double (*initializer) (double, double, double) = { &f1 };

static void
fma4_test (void)
{
    char buffer[256];
    const char *expectation = "4.93038e-32, 4.93038e-32, 4.93038e-32";

    volatile double a = 1.0000000000000002;
    volatile double b = -0.9999999999999998;
    volatile double c = 1.0;

    void *hdl = dlopen(0, RTLD_NOW);

    double (*pf1)(double, double, double) = dlsym(hdl, "f1");
    double (*pk1)(double, double, double, void**) = dlsym(hdl, "k1");
    double (*_pf1)(double, double, double);

    double v1_1 = pf1(a, b, c);
    double v1_2 = pk1(a, b, c, (void**)&_pf1);
    double v1_3 = _pf1(a, b, c);
    __builtin_sprintf (buffer, "%g, %g, %g", v1_1, v1_2, v1_3);
    if (__builtin_strcmp (buffer, expectation) != 0)
      __builtin_abort ();

    double (*pf2)(double, double, double) = dlsym(hdl, "f2");
    double (*pk2)(double, double, double, void**) = dlsym(hdl, "k2");
    double (*_pf2)(double, double, double);

    double v2_1 = pf2(a, b, c);
    double v2_2 = pk2(a, b, c, (void**)&_pf2);
    double v2_3 = _pf2(a, b, c);
    __builtin_sprintf(buffer, "%g, %g, %g", v2_1, v2_2, v2_3);
    if (__builtin_strcmp (buffer, expectation) != 0)
      __builtin_abort ();

    __builtin_sprintf(buffer, "%g, %g, %g", initializer (a, b, c), v2_2, v2_3);
    if (__builtin_strcmp (buffer, expectation) != 0)
      __builtin_abort ();
}
