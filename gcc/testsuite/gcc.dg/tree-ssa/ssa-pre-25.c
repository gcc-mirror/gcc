/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

struct X { int i; };

int foo (int x)
{
  struct X a;
  struct X b;
  struct X *p;
  a.i = 1;
  b.i = 2;
  if (x)
    p = &a;
  else
    p = &b;
  return p->i;
}

/* We should eliminate the load from p for a PHI node with values 1 and 2.  */

/* { dg-final { scan-tree-dump "Eliminated: 1" "pre" } } */
