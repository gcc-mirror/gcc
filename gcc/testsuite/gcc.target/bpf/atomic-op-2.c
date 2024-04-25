/* Test 32-bit non-fetch atomic operations.  */
/* { dg-do compile } */
/* { dg-options "-mv3-atomics -O2 -masm=normal" } */

int val;

void
test_atomic_add (int x)
{
  __atomic_add_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_sub (int x)
{
  __atomic_add_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_and (int x)
{
  __atomic_and_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_nand (int x)
{
  __atomic_nand_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_or (int x)
{
  __atomic_or_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_xor (int x)
{
  __atomic_xor_fetch (&val, x, __ATOMIC_ACQUIRE);
}

/* sub implemented in terms of add, and we output xadd to support older GAS.  */
/* { dg-final { scan-assembler-times "xaddw\t" 2 } } */
/* { dg-final { scan-assembler-times "aand32\t" 1 } } */
/* nand must use an exchange loop */
/* { dg-final { scan-assembler "acmp32\t" } } */
/* { dg-final { scan-assembler-times "aor32\t" 1 } } */
/* { dg-final { scan-assembler-times "axor32\t" 1 } } */
