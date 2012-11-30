/* { dg-do compile } */
/* { dg-options "-O2" } */

int v = 0;

int
atomic_fetch_add_RELEASE (int a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_sub_RELEASE (int a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_and_RELEASE (int a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_nand_RELEASE (int a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_xor_RELEASE (int a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_RELEASE);
}

int
atomic_fetch_or_RELEASE (int a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_RELEASE);
}

/* { dg-final { scan-assembler-times "ldxr\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "stlxr\tw\[0-9\]+, w\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
