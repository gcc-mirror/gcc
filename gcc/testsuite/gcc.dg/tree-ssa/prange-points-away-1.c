/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-alias" } */

int gv, *a, **b = &a;

int *
d (int *p)
{
  if (p == &gv)
    __builtin_unreachable ();
  return p;
}

int
main ()
{
  *b = d (a);
}

/* { dg-final { scan-tree-dump "! -> &gv" "optimized" } } */
