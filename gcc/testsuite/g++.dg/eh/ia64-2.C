// PR target/30230
// This testcase failed on IA-64, where end of an EH region ended
// in the middle of a bundle (with br.call insn in first or second
// slot of .bbb/.mbb bundles and EH region end right after it).
// But br.call returns to the start of the next bundlem so during
// unwinding the call was considered to be outside of the EH region
// while it should have been inside.
// { dg-options "-O2" }
// { dg-do run }

struct A {};
struct B { virtual ~B(); };
B::~B () {}
struct C { void foo (short &, B &); };
struct D { void *d1; C *d2; virtual void virt (void) {} };
struct E { D *e1; B *e2; };
struct F { void bar (void *, B &); };
F *p __attribute__((weak));
volatile int r;

void C::foo (short &x, B &)
{
  if (r)
    throw A ();
  x = 1;
}

void F::bar (void *, B &)
{
  throw A ();
}

void baz (E &x)
{
  short g = 0;
  B b = *x.e2;
  x.e1->d2->foo (g, b);
  if (g)
    p->bar(x.e1->d1, b);
}

int main ()
{
  F g;
  D s;
  E h;
  p = &g;
  h.e1 = &s;
  try
    {
      baz (h);
    }
  catch (A &)
    {
    }
  return 0;
}
