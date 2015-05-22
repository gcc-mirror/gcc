// { dg-options "-std=c++14" }

struct A {
  auto f() & {}
  auto f() && {}
};

void Foo (A &a)
{
  a.f();
}

void Bar ()
{
  A{}.f ();
}
