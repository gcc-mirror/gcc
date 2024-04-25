/* Test atomic exchange instruction is disabled with -mno-v3-atomics.  */
/* { dg-do compile } */
/* { dg-options "-mno-v3-atomics -O2 -masm=normal" } */

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

/* { dg-final { scan-assembler-not "axchg\t.*" } } */
/* { dg-final { scan-assembler-not "axchg32\t.*" } } */
