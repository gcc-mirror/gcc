/* PR tree-optimization/68671 */
/* { dg-do run } */
/* { dg-options " -O2 -fno-tree-dce" } */
/* { dg-xfail-if "ptxas crashes" { nvptx-*-* } } */

volatile int a = -1;
volatile int b;

static inline int
fn1 (signed char p1, int p2)
{
  return (p1 < 0) || (p1 > (1 >> p2)) ? 0 : (p1 << 1);
}

int
main ()
{
  signed char c = a;
  b = fn1 (c, 1);
  c = ((128 | c) < 0 ? 1 : 0);
  if (c != 1)
    __builtin_abort ();
  return 0;
}
