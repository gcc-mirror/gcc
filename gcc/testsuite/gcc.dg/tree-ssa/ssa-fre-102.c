/* PR/111715 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

struct B {
   struct { int len; } l;
   long n;
};
struct A {
   struct B elts[8];
};

static void
set_len (struct B *b, int len)
{
  b->l.len = len;
}

static int
get_len (struct B *b)
{
  return b->l.len;
}

int foo (struct A *a, int i, long *q)
{
  set_len (&a->elts[i], 1);
  *q = 2;
  return get_len (&a->elts[i]);
}

/* { dg-final { scan-tree-dump "return 1;" "fre1" } } */
