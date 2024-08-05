/* Test 32-bit fetch and non-fetch atomic operations.  */
/* { dg-do compile } */
/* { dg-options "-mv3-atomics -O2 -masm=normal" } */

/* Note that GCC optimizes __atomic_add_fetch calls whose return value is not
   used into non-fetching operations that, in BPF, generate fetching
   instructions anyway.  See note in gcc/config/bpf/atomic.md on this
   regard.  */

int val;

void
test_atomic_add (int x)
{
  __atomic_add_fetch (&val, x, __ATOMIC_ACQUIRE);
}

long
test_used_atomic_add (int x)
{
  return __atomic_add_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_sub (int x)
{
  __atomic_add_fetch (&val, x, __ATOMIC_ACQUIRE);
}

long
test_used_atomic_sub (int x)
{
  return __atomic_add_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_and (int x)
{
  __atomic_and_fetch (&val, x, __ATOMIC_ACQUIRE);
}

long
test_used_atomic_and (int x)
{
  return __atomic_and_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_nand (int x)
{
  __atomic_nand_fetch (&val, x, __ATOMIC_ACQUIRE);
}

long
test_used_atomic_nand (int x)
{
  return __atomic_nand_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_or (int x)
{
  __atomic_or_fetch (&val, x, __ATOMIC_ACQUIRE);
}

long
test_used_atomic_or (int x)
{
  return __atomic_or_fetch (&val, x, __ATOMIC_ACQUIRE);
}

void
test_atomic_xor (int x)
{
  __atomic_xor_fetch (&val, x, __ATOMIC_ACQUIRE);
}

long
test_used_atomic_xor (int x)
{
  return __atomic_xor_fetch (&val, x, __ATOMIC_ACQUIRE);
}

/* sub implemented in terms of add.  */
/* { dg-final { scan-assembler-times "afadd32" 4 } } */
/* { dg-final { scan-assembler-times "afand32\t" 2 } } */
/* nand must use an exchange loop */
/* { dg-final { scan-assembler "acmp32\t" } } */
/* { dg-final { scan-assembler-times "afor32\t" 2 } } */
/* { dg-final { scan-assembler-times "afxor32\t" 2 } } */
