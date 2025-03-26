/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */
/* { dg-require-effective-target c99_runtime } */

extern float sinhf (float);
extern float tanhf (float);
extern double sinh (double);
extern double tanh (double);
extern long double sinhl (long double);
extern long double tanhl (long double);

double __attribute__ ((noinline))
tanhbysinh_ (double x)
{
    return tanh (x) / sinh (x);
}

float __attribute__ ((noinline))
tanhbysinhf_ (float x)
{
    return tanhf (x) / sinhf (x);
}

long double __attribute__ ((noinline))
tanhbysinhl_ (long double x)
{
    return tanhl (x) / sinhl (x);
}


/* There must be no calls to sinh or atanh */
/* There must be calls to cosh */
/* { dg-final { scan-tree-dump-not "sinh " "optimized" } } */
/* { dg-final { scan-tree-dump-not "tanh " "optimized" } } */
/* { dg-final { scan-tree-dump-not "sinhf " "optimized" } } */
/* { dg-final { scan-tree-dump-not "tanhf " "optimized" } } */
/* { dg-final { scan-tree-dump-not "sinhl " "optimized" } } */
/* { dg-final { scan-tree-dump-not "tanhl " "optimized" } } */
/* { dg-final { scan-tree-dump "cosh " "optimized" } } */
/* { dg-final { scan-tree-dump "coshf " "optimized" } } */
/* { dg-final { scan-tree-dump "coshl " "optimized" } } */
