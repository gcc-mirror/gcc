/* { dg-do compile } */
/* { dg-options "-O2" } */

char v = 0;

char
atomic_fetch_add_RELAXED (char a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_sub_RELAXED (char a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_and_RELAXED (char a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_nand_RELAXED (char a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_xor_RELAXED (char a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_RELAXED);
}

char
atomic_fetch_or_RELAXED (char a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_RELAXED);
}

/* { dg-final { scan-assembler-times "ldxrb\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "stxrb\tw\[0-9\]+, w\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
