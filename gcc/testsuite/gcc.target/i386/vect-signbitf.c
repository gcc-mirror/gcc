/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-O2 -msse2 -ftree-vectorize -fdump-tree-vect-details -save-temps" } */

extern void abort ();

#define N 1024
float a[N] = {0.0f, -0.0f, 1.0f, -1.0f,
	      -2.0f, 3.0f, -5.0f, -8.0f,
	      13.0f, 21.0f, -25.0f, 33.0f};
int r[N];

int
main (void)
{
  int i;

  for (i = 0; i < N; i++)
    r[i] = __builtin_signbitf (a[i]);

  /* check results:  */
  for (i = 0; i < N; i++)
    if (__builtin_signbitf (a[i]) && !r[i])
      abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-assembler-not "\\$-2147483648" } } */
/* { dg-final { scan-assembler "psrld" } } */
