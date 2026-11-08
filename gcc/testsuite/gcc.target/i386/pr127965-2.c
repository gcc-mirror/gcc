/* PR tree-optimization/127965 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2 -masm=att" } */

int x = 0x20;

void
foo ()
{
  int v = __atomic_fetch_or (&x, 0x20, __ATOMIC_SEQ_CST);
  if ((_BitInt (6)) v >= 0)
    __builtin_abort ();
}

/* { dg-final { scan-assembler "lock;?\[ \t\]*btsl" } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
