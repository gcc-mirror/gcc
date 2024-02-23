/* PR rtl-optimization/114054 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-Og -fwhole-program -fno-tree-ccp -fprofile-use -fno-tree-copy-prop -w" } */

int x;

void
foo (int i, unsigned u)
{
  x = __builtin_mul_overflow_p ((unsigned _BitInt(1)) u, i, (_BitInt(33)) 0);
}

int
main ()
{
  foo (11, 0);
}
