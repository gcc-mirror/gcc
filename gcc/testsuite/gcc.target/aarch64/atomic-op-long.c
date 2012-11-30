/* { dg-do compile } */
/* { dg-options "-O2" } */

long v = 0;

long
atomic_fetch_add_RELAXED (long a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_RELAXED);
}

long
atomic_fetch_sub_RELAXED (long a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_RELAXED);
}

long
atomic_fetch_and_RELAXED (long a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_RELAXED);
}

long
atomic_fetch_nand_RELAXED (long a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_RELAXED);
}

long
atomic_fetch_xor_RELAXED (long a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_RELAXED);
}

long
atomic_fetch_or_RELAXED (long a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_RELAXED);
}

/* { dg-final { scan-assembler-times "ldxr\tx\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "stxr\tw\[0-9\]+, x\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
