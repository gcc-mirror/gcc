/* { dg-do compile } */
/* { dg-options "-O2" } */

int v = 0;

int
atomic_fetch_add_CONSUME (int a)
{
  return __atomic_fetch_add (&v, a, __ATOMIC_CONSUME);
}

int
atomic_fetch_sub_CONSUME (int a)
{
  return __atomic_fetch_sub (&v, a, __ATOMIC_CONSUME);
}

int
atomic_fetch_and_CONSUME (int a)
{
  return __atomic_fetch_and (&v, a, __ATOMIC_CONSUME);
}

int
atomic_fetch_nand_CONSUME (int a)
{
  return __atomic_fetch_nand (&v, a, __ATOMIC_CONSUME);
}

int
atomic_fetch_xor_CONSUME (int a)
{
  return __atomic_fetch_xor (&v, a, __ATOMIC_CONSUME);
}

int
atomic_fetch_or_CONSUME (int a)
{
  return __atomic_fetch_or (&v, a, __ATOMIC_CONSUME);
}

/* { dg-final { scan-assembler-times "ldxr\tw\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
/* { dg-final { scan-assembler-times "stxr\tw\[0-9\]+, w\[0-9\]+, \\\[x\[0-9\]+\\\]" 6 } } */
