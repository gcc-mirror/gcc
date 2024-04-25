/* Test atomic exchange instruction.  */
/* { dg-do compile } */
/* { dg-options "-mv3-atomics -O2 -masm=normal" } */

int foo (int *p, int *new)
{
  int old;
  __atomic_exchange (p, new, &old, __ATOMIC_RELAXED);
  return old;
}

int foo64 (long *p, long *new)
{
  long old;
  __atomic_exchange (p, new, &old, __ATOMIC_SEQ_CST);
  return old;
}

/* { dg-final { scan-assembler-times "axchg\t.*" 1 } } */
/* { dg-final { scan-assembler-times "axchg32\t.*" 1 } } */
