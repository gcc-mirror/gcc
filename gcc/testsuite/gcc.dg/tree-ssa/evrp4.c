/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-details" } */

int foo (int *p);

struct st
{
  int a;
  int b;
};

int bar (struct st *s)
{

  if (!s)
    return 0;
  foo (&s->a);
}

/* { dg-final { scan-tree-dump "\\\[1B, \\+INF\\\]" "evrp" } } */
