/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct param { int *a; };
void foo(struct param *p);
int bar(void)
{
  int a[2];
  struct param p;
  a[0] = 1;
  a[1] = 1;
  p.a = &a[0];
  foo(&p);
  return a[0] + *p.a;
}

/* { dg-final { scan-tree-dump "return \\*p\\.a \\\+ a.0.;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

