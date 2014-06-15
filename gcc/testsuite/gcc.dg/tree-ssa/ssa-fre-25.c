/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-fre1" } */

struct X { int i; int j; };
void bar (struct X *);
int foo (struct X *p)
{
  struct X x;
  p->i = 1;
  x = *p;
  x.j = 2;
  return p->i - x.i;
}

/* We should optimize this to return 0.  */

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
