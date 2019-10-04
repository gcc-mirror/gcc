/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */

extern float sinhf (float);
extern float coshf (float);
extern float tanhf (float);
extern float sqrtf (float);
extern double sinh (double);
extern double cosh (double);
extern double sqrt (double);
extern double tanh (double);
extern long double sinhl (long double);
extern long double coshl (long double);
extern long double tanhl (long double);
extern long double sqrtl (long double);

double __attribute__ ((noinline))
sinhovercosh_ (double x)
{
  return sinh (x) / cosh (x);
}

float __attribute__ ((noinline))
sinhfovercoshf_(float x)
{
  return sinhf (x) / coshf (x);
}

long double __attribute__ ((noinline))
sinhlovercoshl_ (long double x)
{
  return sinhl (x) / coshl (x);
}

/* There must be no calls to sinh, cosh, or atanh */
/* {dg-final { scan-tree-dump-not "sinh " "optimized" } } */
/* {dg-final { scan-tree-dump-not "cosh " "optimized" } } */
/* {dg-final { scan-tree-dump-not "sinfh " "optimized" } } */
/* {dg-final { scan-tree-dump-not "cosfh " "optimized" } } */
/* {dg-final { scan-tree-dump-not "sinlh " "optimized" } } */
/* {dg-final { scan-tree-dump-not "coslh " "optimized" } } */
/* {dg-final { scan-tree-dump-times "tanh " "1" "optimized" }} */
/* {dg-final { scan-tree-dump-times "tanhl " "1" "optimized" }} */
/* {dg-final { scan-tree-dump-times "tanhf " "1" "optimized" }} */

