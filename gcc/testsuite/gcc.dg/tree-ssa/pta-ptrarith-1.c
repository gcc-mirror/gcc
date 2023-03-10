/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-forwprop -fno-tree-ccp -fdump-tree-ealias-details" } */

extern void abort (void);
struct X {
  int *p;
  int *q;
  int *r;
};
int __attribute__((noinline))
foo(int i, int j, int k, int off)
{
  struct X x;
  int **p, *q;
  x.p = &i;
  x.q = &j;
  x.r = &k;
  p = &x.q;
  p += 1;
  /* *p points to { k } */
  q = *p;
  return *q;
}

/* { dg-final { scan-tree-dump "q_. = { k }" "ealias" } } */
