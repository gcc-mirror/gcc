/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */

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

float __attribute__ ((noinline))
coshatanhf_(float x)
{
    float atg = atanhf(x);
    return coshf(atg) + atg;
}

double __attribute__ ((noinline))
cosatan_(double x)
{
    double atg = atanh(x);
    return cosh(atg) + atg;
}

long double __attribute__ ((noinline))
cosatanl_(long double x)
{
    long double atg = atanhl(x);
    return coshl(atg) + atg;
}

float __attribute__ ((noinline))
sinatanf_(float x)
{
    float atg = atanhf(x);
    return sinhf(atg) + atg;
}

double __attribute__ ((noinline))
sinatan_(double x)
{
    double atg = atanh(x);
    return sinh(atg) + atg;
}

long double __attribute__ ((noinline))
sinatanl_(long double x)
{
    long double atg = atanhl(x);
    return sinhl(atg) + atg;
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
