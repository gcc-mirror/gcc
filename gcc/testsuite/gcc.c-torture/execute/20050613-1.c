/* PR tree-optimization/22043 */

extern void abort (void);

struct A { int i; int j; int k; int l; };
struct B { struct A a; int r[1]; };
struct C { struct A a; int r[0]; };
struct D { struct A a; int r[]; };

void
foo (struct A *x)
{
  if (x->i != 0 || x->j != 5 || x->k != 0 || x->l != 0)
    abort ();
}

int
main ()
{
  struct B b = { .a.j = 5 };
  struct C c = { .a.j = 5 };
  struct D d = { .a.j = 5 };
  foo (&b.a);
  foo (&c.a);
  foo (&d.a);
  return 0;
}
