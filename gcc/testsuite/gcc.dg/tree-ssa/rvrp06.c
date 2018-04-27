/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-rvrp" } */

void f(unsigned);

void
f4 (unsigned a, unsigned b)
{
  if (a <= 5 && b <= 2)
    {
      int x = a * b * 4;
      if (x > 40)
        f (a);
      if (x >= 0)
        f (b);
      else
        f (a + b);
    }
}

/* { dg-final { scan-tree-dump "Branch rewritten.*1 !=" "rvrp" } } */
/* { dg-final { scan-tree-dump "Branch rewritten.*0 !=" "rvrp" } } */
