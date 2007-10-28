/* { dg-do compile } */
/* { dg-options "-O -fno-early-inlining -fdump-tree-salias-vops-details" } */

struct foo {
  int a;
  struct X {
    int b[4];
  } b;
} m;
static inline struct X *wrap(struct X *p) { return p; }
int test2(void)
{
  struct X *p = wrap(&m.b);
  /* Both memory references need to alias the same SFT.  */
  return p->b[3] - m.b.b[3];
}

/* { dg-final { scan-tree-dump "SFT.5 created for var m offset 128" "salias" } } */
/* { dg-final { scan-tree-dump-times "VUSE <SFT.5_" 2 "salias" } } */
/* { dg-final { cleanup-tree-dump "salias" } } */
