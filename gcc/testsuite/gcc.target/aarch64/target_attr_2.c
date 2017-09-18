/* { dg-do assemble } */
/* { dg-options "-O2 -mcpu=cortex-a57 -march=armv8-a -ftree-vectorize -fdump-tree-vect-all" } */

/* The various ways to turn off simd availability should
   turn off vectorization.  */

__attribute__ ((target ("+nosimd")))
int
baz (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] += 5;
}

__attribute__ ((target ("arch=armv8-a+nosimd")))
int
baz2 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] += 5;
}

__attribute__ ((target ("cpu=cortex-a53+nosimd")))
int
baz3 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] += 5;
}

__attribute__ ((target ("general-regs-only")))
int
baz4 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] += 5;
}

/* { dg-final { scan-tree-dump-not "vectorized 1 loops" "vect" } } */
