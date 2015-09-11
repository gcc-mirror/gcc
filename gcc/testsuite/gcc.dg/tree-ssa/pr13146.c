/* { dg-do compile } */
/* { dg-options "-O -fstrict-aliasing -fdump-tree-optimized" } */

struct A
{
  int i;
};
struct B
{
  struct A a;
  int j;
};

int foo (struct A *p, struct B *q)
{
  p->i = 0;
  q->j = 1;
  return p->i;
}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
