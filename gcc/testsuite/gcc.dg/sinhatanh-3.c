/* { dg-do compile } */
/* { dg-options "-Ofast -fmath-errno -fdump-tree-optimized" } */

extern float sinhf (float);
extern float coshf (float);
extern float atanhf (float);
extern float sqrtf (float);
extern double sinh (double);
extern double cosh (double);
extern double atanh (double);
extern double sqrt (double);
extern long double sinhl (long double);
extern long double coshl (long double);
extern long double atanhl (long double);
extern long double sqrtl (long double);

double __attribute__ ((noinline))
sinhatanh_ (double x)
{
    return sinh (atanh (x));
}

double __attribute__ ((noinline))
coshatanh_ (double x)
{
    return cosh (atanh (x));
}

float __attribute__ ((noinline))
sinhatanhf_(float x)
{
    return sinhf (atanhf (x));
}

float __attribute__ ((noinline))
coshatanhf_(float x)
{
    return coshf (atanhf (x));
}

long double __attribute__ ((noinline))
sinhatanhl_ (long double x)
{
    return sinhl (atanhl (x));
}

long double __attribute__ ((noinline))
coshatanhl_ (long double x)
{
    return coshl (atanhl (x));
}

/* There should be calls to sinh, cosh and atanh */
/* { dg-final { scan-tree-dump-times "cosh " "1" "optimized" } } */
/* { dg-final { scan-tree-dump-times "sinh " "1" "optimized" } } */
/* { dg-final { scan-tree-dump-times "atanh " "2" "optimized" } } */
/* { dg-final { scan-tree-dump-times "coshf " "1" "optimized" } } */
/* { dg-final { scan-tree-dump-times "sinhf " "1" "optimized" } } */
/* { dg-final { scan-tree-dump-times "atanhf " "2" "optimized" } } */
/* { dg-final { scan-tree-dump-times "coshl " "1" "optimized" } } */
/* { dg-final { scan-tree-dump-times "sinhl " "1" "optimized" } } */
/* { dg-final { scan-tree-dump-times "atanhl " "2" "optimized" } } */
