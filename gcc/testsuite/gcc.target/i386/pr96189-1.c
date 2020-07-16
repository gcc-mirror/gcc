/* PR target/96176 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "\tcmpb\t" } } */

_Bool
foo (short *x, short z)
{
  short y = 0;
  __atomic_compare_exchange_n (x, &y, z, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
  return y == 0;
}

/* { dg-final { scan-assembler-not "\ttestw\t" } } */

_Bool
bar (short *x, short z)
{
  short y = -1;
  __atomic_compare_exchange_n (x, &y, z, 0, __ATOMIC_RELAXED, __ATOMIC_RELAXED);
  return y == -1;
}

/* { dg-final { scan-assembler-not "\tincw\t" } } */
