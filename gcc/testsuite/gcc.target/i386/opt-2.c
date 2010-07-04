/* Test the attribute((optimize)) really works.  Do this test by checking
   whether we vectorize a simple loop.  */
/* { dg-do compile } */
/* { dg-options "-O1 -msse2 -mfpmath=sse -march=k8" } */
/* { dg-require-effective-target sse2 } */
/* { dg-final { scan-assembler "prefetcht0" } } */
/* { dg-final { scan-assembler "addps" } } */
/* { dg-final { scan-assembler "subss" } } */

#define SIZE 10240
float a[SIZE] __attribute__((__aligned__(32)));
float b[SIZE] __attribute__((__aligned__(32)));
float c[SIZE] __attribute__((__aligned__(32)));

/* This should vectorize.  */
#pragma GCC push_options
#pragma GCC optimize (3, "unroll-all-loops", "-fprefetch-loop-arrays")

void
opt3 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] + c[i];
}

#pragma GCC pop_options

/* This should not vectorize.  */
void
not_opt3 (void)
{
  int i;

  for (i = 0; i < SIZE; i++)
    a[i] = b[i] - c[i];
}

