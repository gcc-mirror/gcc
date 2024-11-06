/* Mismatching decl of foo.  */
/* { dg-additional-options "-std=gnu17" { target c } } */

int foo ();

int bar (void)
{
  return foo() + 1;
}

int foo (int x, int y)
{
  return x * y;
}
