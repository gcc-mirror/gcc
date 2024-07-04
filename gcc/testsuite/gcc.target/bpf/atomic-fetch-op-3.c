/* Test atomic-fetch-op instructions are disabled with -mno-v3-atomics.  */

/* { dg-do compile } */
/* { dg-options "-mno-v3-atomics -O2 -masm=normal" } */

long val;

long
test_atomic_fetch_add (long x)
{
  return __atomic_fetch_add (&val, x, __ATOMIC_ACQUIRE);
}

long
test_atomic_fetch_sub (long x)
{
  return __atomic_fetch_sub (&val, x, __ATOMIC_RELEASE);
}

long
test_atomic_fetch_and (long x)
{
  return __atomic_fetch_and (&val, x, __ATOMIC_ACQUIRE);
}

long
test_atomic_fetch_nand (long x)
{
  return __atomic_fetch_nand (&val, x, __ATOMIC_ACQUIRE);
}

long
test_atomic_fetch_or (long x)
{
  return __atomic_fetch_or (&val, x, __ATOMIC_ACQUIRE);
}

long
test_atomic_fetch_xor (long x)
{
  return __atomic_fetch_xor (&val, x, __ATOMIC_ACQUIRE);
}

/* { dg-final { scan-assembler-not "afadd\t" } } */
/* { dg-final { scan-assembler-not "afand\t" } } */
/* { dg-final { scan-assembler-not "afor\t" } } */
/* { dg-final { scan-assembler-not "afxor\t" } } */
/* { dg-final { scan-assembler-not "acmp\t" } } */
/* { dg-final { scan-assembler-not "axchg\t" } } */
