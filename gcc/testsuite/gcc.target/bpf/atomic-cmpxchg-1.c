/* { dg-do compile } */
/* { dg-options "-mv3-atomics -O2 -masm=normal" } */

int
foo (int *p, int *expected, int desired)
{
  return __atomic_compare_exchange (p, expected, &desired, 0,
				    __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
}

int
foo64 (long *p, long *expected, long desired)
{
  return __atomic_compare_exchange (p, expected, &desired, 0,
				    __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
}

/* { dg-final { scan-assembler "acmp\t.*" } } */
/* { dg-final { scan-assembler "acmp32\t.*" } } */
