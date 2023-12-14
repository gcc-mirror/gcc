/* Mismatching decl of foo.  */

int foo ();

int bar (void)
{
  return foo() + 1;
}

int foo (int x, int y)
{
  return x * y;
}
