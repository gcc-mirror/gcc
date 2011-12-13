/* { dg-do compile } */
/* { dg-options "-O -fipa-cp -fipa-cp-clone" } */

int
baz (void)
{
  return 0;
}

int make_mess;

__attribute__ ((noinline))
int bar (int x, int (*f) (void))
{
  return f ();
}

int
foo (void)
{
  return bar (1, baz);
}
