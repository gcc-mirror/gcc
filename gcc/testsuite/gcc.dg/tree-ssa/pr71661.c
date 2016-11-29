/* { dg-do run } */
/* { dg-options "-O3 -fdisable-tree-ethread" } */

extern void exit (int);

int a, b;

void
fn1 ()
{
  unsigned c = 0;
  int d;
  b = a;
  if (a < 0)
    goto L1;
  for (; a < 1; a++)
    d = 0;
  for (; d < 2; d++)
    {
      for (c = 0; c < 3; c++)
      L1:
        a = 2;
    }
}

int
main ()
{
  fn1 ();
  exit (0);
}
