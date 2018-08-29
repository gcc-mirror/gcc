// PR c++/84874
// { dg-do run { target c++17 } }
// { dg-options "" }

struct A { int a; struct { int b; }; };
struct B { A d; };

void
foo (B *x)
{
  *x = { .d = { .b = 5 } };
}

void
bar (A *x)
{
  *x = { .b = 6 };
}

int
main ()
{
  B b = { { 2, 3 } };
  foo (&b);
  if (b.d.a != 0 || b.d.b != 5)
    __builtin_abort ();
  b.d.a = 8;
  bar (&b.d);
  if (b.d.a != 0 || b.d.b != 6)
    __builtin_abort ();
}
