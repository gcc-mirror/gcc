/* { dg-do compile } */
/* We don't (and don't want to) perform this optimisation on soft-float
   targets, where each addition is a library call.  */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -fno-tree-vectorize -funroll-loops --fast-math -fvariable-expansion-in-unroller -fdump-rtl-loop2_unroll" } */
/* { dg-additional-options "--param max-completely-peel-times=16  --param max-unroll-times=8" { target s390*-*-* } } */

extern void abort (void);

float array[30] = { 1,2,3,4,5,6,7,8,9,10 };

int foo (int n)
{
  unsigned i;
  float accum = 1.0;

  for (i = 0; i < n; i++)
    accum += array[i];

  return accum;
}

int main (void)
{
  return foo (10);
}

/* { dg-final { scan-rtl-dump "Expanding Accumulator" "loop2_unroll" } } */
