/* { dg-do run } */
/* { dg-options "-O2 -fipa-pta -fno-tree-fre -fno-tree-sra -fdump-ipa-pta-details" } */

struct X {
    int i;
    void *p;
};

static void * __attribute__((noinline,noclone))
foo(struct X *q, void *p)
{
  struct X b;
  b.p = p;
  *q = b;
  return q->p;
}
extern void abort (void);
int main()
{
  struct X a, c;
  void *p;
  a.p = (void *)&c;
  p = foo(&a, &a);
  /* { dg-final { scan-ipa-dump "foo.result = { NULL a\[^ \]* c\[^ \]* }" "pta" { xfail *-*-* } } } */
  /* { dg-final { scan-ipa-dump "foo.result = { NULL a\[^ \]* a\[^ \]* c\[^ \]* }" "pta" } } */
  ((struct X *)p)->p = (void *)0;
  if (a.p != (void *)0)
    abort ();
  return 0;
}

/* { dg-final { cleanup-ipa-dump "pta" } } */
