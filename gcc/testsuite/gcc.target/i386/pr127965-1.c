/* PR tree-optimization/127965 */
/* { dg-do compile } */
/* { dg-options "-O2 -masm=att" } */

int x = (unsigned char) (-__SCHAR_MAX__ - 1);

void
foo ()
{
  int v = __atomic_fetch_or (&x, (unsigned char) (-__SCHAR_MAX__ - 1), __ATOMIC_SEQ_CST);
  if ((signed char) v >= 0)
    __builtin_abort ();
}

/* { dg-final { scan-assembler "lock;?\[ \t\]*btsl" } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
