/* { dg-do compile } */
/* { dg-require-effective-target large_double } */
/* { dg-require-effective-target c99_runtime } */
/* { dg-options "-O2 -ffast-math -fdump-tree-forwprop1-details" } */

extern double fabs (double);

double f(float f)
{
  double t1 = fabs(f); 
  double t2 = f / t1;
  return t2;
}

/* { dg-final { scan-tree-dump "__builtin_copysign" "forwprop1" } } */
