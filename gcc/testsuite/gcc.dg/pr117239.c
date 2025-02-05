/* PR rtl-optimization/117239 */
/* { dg-do run } */
/* { dg-options "-fno-inline -O2" } */
/* { dg-additional-options "-fschedule-insns" { target i?86-*-* x86_64-*-* } } */

int a, b, c = 1, d;

int
foo (void)
{
  return a;
}

struct A {
  int e, f, g, h;
  short i;
  int j;
};

void
bar (int x, struct A y)
{
  if (y.j == 1)
    c = 0;
}

int
baz (struct A x)
{
  return b;
}

int
main ()
{
  struct A k = { 0, 0, 0, 0, 0, 1 };
  d = baz (k);
  bar (foo (), k);
  if (c != 0)
    __builtin_abort ();
  return 0;
}
