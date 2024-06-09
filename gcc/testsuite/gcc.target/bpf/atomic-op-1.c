/* Test 64-bit non-fetch atomic operations.  */
/* { dg-do compile } */
/* { dg-options "-mv3-atomics -O2 -masm=normal" } */

long val;

void
test_atomic_add (long x)
{
  __atomic_add_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_sub (long x)
{
  __atomic_add_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_and (long x)
{
  __atomic_and_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_nand (long x)
{
  __atomic_nand_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_or (long x)
{
  __atomic_or_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_xor (long x)
{
  __atomic_xor_fetch (&val, x, __ATOMIC_ACQUIRE);
}

/* sub implemented in terms of add, and we output xadd to support older GAS.  */
/* { dg-final { scan-assembler-times "xadddw\t" 2 } } */
/* { dg-final { scan-assembler-times "aand\t" 1 } } */
/* nand must use an exchange loop */
/* { dg-final { scan-assembler "acmp\t" } } */
/* { dg-final { scan-assembler-times "aor\t" 1 } } */
/* { dg-final { scan-assembler-times "axor\t" 1 } } */
