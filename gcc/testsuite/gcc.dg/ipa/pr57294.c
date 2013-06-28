/* { dg-do compile } */
/* { dg-options "-O3" } */

void baz (void);
int func ();

static void
bar (int a, int foo (void))
{
  baz ();
  foo ();
}

void
baz (void)
{
  bar (0, func);
}
