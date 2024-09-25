/* { dg-do compile } */
/* { dg-options "-mv3-atomics -O2 -masm=pseudoc" } */

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

int
bar (int *p, int *new)
{
  int old;
  __atomic_exchange (p, new, &old, __ATOMIC_RELAXED);
  return old;
}

int
bar64 (long *p, long *new)
{
  long old;
  __atomic_exchange (p, new, &old, __ATOMIC_SEQ_CST);
  return old;
}

/* { dg-final { scan-assembler "r. = cmpxchg_64\\(r.\\+0, r., r.\\)" } } */
/* { dg-final { scan-assembler "w. = cmpxchg32_32\\(r.\\+0, w., w.\\)" } } */
/* { dg-final { scan-assembler-times "w. = xchg32_32\\(r.\\+0, w.\\)" 1 } } */
/* { dg-final { scan-assembler-times "r. = xchg_64\\(r.\\+0, r.\\)" 1 } } */

