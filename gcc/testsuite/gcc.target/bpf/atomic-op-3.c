/* Test that atomic insns are properly disabled with -mno-v3-atomics.  */
/* { dg-do compile } */
/* { dg-options "-mno-v3-atomics -O2 -masm=normal" } */

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

/* Without v3 atomics, only xadd{w,dw} is available.  */
/* { dg-final { scan-assembler-not "aadd" } } */
/* { dg-final { scan-assembler-not "aand" } } */
/* { dg-final { scan-assembler-not "aor" } } */
/* { dg-final { scan-assembler-not "axor" } } */
/* { dg-final { scan-assembler-not "axchg" } } */
/* { dg-final { scan-assembler-not "acmp" } } */
