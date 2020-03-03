/* { dg-do run } */
/* { dg-additional-options "-fno-tree-dse" } */

union U { long long i; long f; };
struct a {union U u;};
struct aa {struct a a;};
struct b {union U u;};
struct bb {struct b b;};

long __attribute__((noipa))
foo (struct bb *bv, void *ptr)
{
  struct aa *a = ptr;
  struct bb *b = ptr;
  bv->b.u.f = 1;
  a->a.u.i = 0;
  b->b.u.f = 0;
  return bv->b.u.f;
}

int
main ()
{
  union C {struct aa aa; struct bb bb;} v;
  if (foo (&v.bb, &v) != 0)
    __builtin_abort ();
  return 0;
}
