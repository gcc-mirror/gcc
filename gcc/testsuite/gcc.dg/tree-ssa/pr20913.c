/* PR tree-optimization/20913
   COPY-PROP did not fold COND_EXPR, blocking some copy propagation
   opportunities.  */

/* { dg-do link } */
/* { dg-options "-O2 -fno-tree-dominator-opts" } */

int
foo (int a, int b, int c, int d)
{
  int x, y;

  b = a;
  if (a == b)
    x = c;
  else
    {
      link_error ();
      x = d;
    }

  if (x == c)
    return a;
  else
    {
      link_error ();
      return b;
    }
}

main()
{
  foo (1, 2, 3, 4);
}
