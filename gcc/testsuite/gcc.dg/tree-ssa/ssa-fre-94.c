/* PR tree-optimization/79333 */
/* { dg-do compile } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O -ffinite-math-only -fdump-tree-fre1" } */

extern __inline __attribute__ ((__always_inline__,__gnu_inline__))
double __attribute__ ((__nothrow__ , __leaf__))
fabs (double __x) { return __builtin_fabs (__x); }

double f(float f)
{
  double t1 = fabs(f);
  double t2 = f / t1;
  return t2;
}

/* { dg-final { scan-tree-dump "copysign" "fre1" } } */
