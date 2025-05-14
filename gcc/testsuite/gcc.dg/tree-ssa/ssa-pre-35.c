/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

void bar (int *);

struct X { int a[2]; };
void foo (struct X *p, int b)
{
  if (b)
    bar ((int *)p + 1);
  bar (&p->a[1]);
}

/* We should PRE and hoist &p->a[1] as (int *)p + 1.  */
/* { dg-final { scan-tree-dump "HOIST inserted: 1" "pre" } } */
