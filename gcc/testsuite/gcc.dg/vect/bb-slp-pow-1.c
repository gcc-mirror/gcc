/* { dg-additional-options "-fno-math-errno -fdisable-tree-sincos" } */
/* { dg-require-effective-target vect_float } */

void __attribute__ ((noipa))
f (float *a)
{
  a[0] = a[0] * a[0];
  a[1] = __builtin_powf (a[1], 2);
  a[2] = a[2] * a[2];
  a[3] = __builtin_powf (a[3], 2);
}

float a[4] = { 1, 2, 3, 4 };

int
main (void)
{
  f (a);
  for (int i = 0; i < 4; ++i)
    {
      if (a[i] != (i + 1) * (i + 1))
	__builtin_abort ();
      asm volatile ("" ::: "memory");
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "basic block vectorized" 1 "slp2" } } */
