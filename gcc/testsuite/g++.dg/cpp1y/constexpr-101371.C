// PR c++/101371
// { dg-do compile { target c++14 } }

struct A {
  int i;
};
struct B {
  A a{};
  constexpr B() : a() {}
  constexpr B(const B &rhs) : a(rhs.a) {}
};
struct C {
  B arr[1];
};

struct X {
  constexpr C fn () const
  {
    C c{};
    return c;
  }
};

void
g ()
{
  X x;
  constexpr auto z = x.fn();
}
