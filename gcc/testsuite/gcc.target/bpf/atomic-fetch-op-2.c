/* Test 32-bit atomic-fetch-op instructions.  */

/* { dg-do compile } */
/* { dg-options "-mv3-atomics -O2 -masm=normal" } */

int val;

int
test_atomic_fetch_add (int x)
{
  return __atomic_fetch_add (&val, x, __ATOMIC_ACQUIRE);
}

int
test_atomic_fetch_sub (int x)
{
  return __atomic_fetch_sub (&val, x, __ATOMIC_RELEASE);
}

int
test_atomic_fetch_and (int x)
{
  return __atomic_fetch_and (&val, x, __ATOMIC_ACQUIRE);
}

int
test_atomic_fetch_nand (int x)
{
  return __atomic_fetch_nand (&val, x, __ATOMIC_ACQUIRE);
}

int
test_atomic_fetch_or (int x)
{
  return __atomic_fetch_or (&val, x, __ATOMIC_ACQUIRE);
}

int
test_atomic_fetch_xor (int x)
{
  return __atomic_fetch_xor (&val, x, __ATOMIC_ACQUIRE);
}

/* sub implemented in terms of add */
/* { dg-final { scan-assembler-times "afadd32\t" 2 } } */
/* { dg-final { scan-assembler-times "afand32\t" 1 } } */
/* nand must use a compare-exchange loop */
/* { dg-final { scan-assembler "acmp32\t" } } */
/* { dg-final { scan-assembler-times "afor32\t" 1 } } */
/* { dg-final { scan-assembler-times "afxor32\t" 1 } } */
