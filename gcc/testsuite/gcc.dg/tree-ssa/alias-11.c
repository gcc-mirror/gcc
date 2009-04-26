/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

struct param { int *a; };
void foo(struct param *p);
int bar(void)
{
  int a[32];
  struct param p;
  a[0] = 1;
  a[1] = 1;
  p.a = &a[0];
  foo(&p);
  return a[0] + *p.a;
}

/* We need to have both: a load from "a[0]" and a load from "*p.a",
   the latter can be an ssa temporary.  */
/* { dg-final { scan-tree-dump "= a.0.;" "optimized" } } */
/* { dg-final { scan-tree-dump "= \\*\[pD\]" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
