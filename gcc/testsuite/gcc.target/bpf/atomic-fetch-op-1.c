/* Test 64-bit atomic-fetch-op instructions.  */

/* { dg-do compile } */
/* { dg-options "-mv3-atomics -O2 -masm=normal" } */

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

/* sub implemented in terms of add */
/* { dg-final { scan-assembler-times "afadd\t" 2 } } */
/* { dg-final { scan-assembler-times "afand\t" 1 } } */
/* nand must use a compare-exchange loop */
/* { dg-final { scan-assembler "acmp\t" } } */
/* { dg-final { scan-assembler-times "afor\t" 1 } } */
/* { dg-final { scan-assembler-times "afxor\t" 1 } } */
