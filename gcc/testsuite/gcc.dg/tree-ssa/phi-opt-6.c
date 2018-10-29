/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt2" } */

struct C { int i; };
int *g(struct C *p)
{
  if (p)
    return &p->i;
  return (int *)0;
}

/* { dg-final { scan-tree-dump-not "if" "phiopt2" } } */
