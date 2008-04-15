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
  /* Both memory references need to alias the same tags.  */
  return p->b[3] - m.b.b[3];
}

/* { dg-final { scan-tree-dump-times "VUSE <m_.\\\(D\\\)>" 2 "salias" } } */
/* { dg-final { cleanup-tree-dump "salias" } } */
