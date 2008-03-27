/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

/* Check that we constant propagate &&c into the goto and remove
   the unreachable BBs.  */

void a(int*);  void b(int*);  void c(int*);  void d(int*);
void func2(int* val)
{
  const void *const labels[] = { &&a, &&b, &&c, &&d };
  goto *labels[2];
  a: a(val);
  b: b(val);
  c: c(val);
  d: d(val);
}

/* { dg-final { scan-tree-dump-not "a \\\(" "ccp1" } } */
/* { dg-final { scan-tree-dump-not "b \\\(" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
