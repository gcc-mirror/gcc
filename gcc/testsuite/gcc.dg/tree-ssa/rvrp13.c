/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-evrp -fdump-tree-rvrp -fdelete-null-pointer-checks" } */

int g, h;

int
foo (int a)
{
  int *p;

  if (a)
    p = &g;
  else
    p = &h;

  if (p != 0)
    return 1;
  else
    return 0;
}

/* { dg-final { scan-tree-dump "Branch rewritten"  "rvrp" { target { ! keeps_null_pointer_checks } } } } */

