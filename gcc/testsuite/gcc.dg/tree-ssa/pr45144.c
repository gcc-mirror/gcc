/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void baz (unsigned);

extern unsigned buf[];

struct A
{
  unsigned a1:10;
  unsigned a2:3;
  unsigned:19;
};

union TMP
{
  struct A a;
  unsigned int b;
};

static unsigned
foo (struct A *p)
{
  union TMP t;
  struct A x;
  
  x = *p;
  t.a = x;
  return t.b;
}

void
bar (unsigned orig, unsigned *new)
{
  struct A a;
  union TMP s;

  s.b = orig;
  a = s.a;
  if (a.a1)
    baz (a.a2);
  *new = foo (&a);
}

/* { dg-final { scan-tree-dump "x = a;" "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
