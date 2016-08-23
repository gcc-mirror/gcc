/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

struct B { int x; };
extern void g3(struct B *that)  __attribute__((nonnull));
int f3(struct B *a)
{
  g3(a);
  return a != (void *)0;
}

/* { dg-final { scan-tree-dump "return 1;" "vrp1" } } */
