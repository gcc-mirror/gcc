/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
struct a {
  int val;
};
struct b {
  struct a a[10],a2[10];
};
struct c {
  struct b b[10];
} *cptr;

struct d {struct c c;} *dptr;

int
test (int i, int j, int k, int l)
{
  cptr->b[i].a[j].val=123;
  dptr->c.b[k].a2[l].val=2;
  return cptr->b[i].a[j].val;
}
/* { dg-final { scan-tree-dump-times "return 123" 1 "fre1"} } */
