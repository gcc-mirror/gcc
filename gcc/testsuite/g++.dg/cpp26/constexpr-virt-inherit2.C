// C++26 P3533R2 - constexpr virtual inheritance
// { dg-do compile { target c++26 } }

struct A { int a; };
struct B { int b; };
struct C : virtual public A, B { int c; };

constexpr C
foo ()
{
  C c;
  c.a = 1;
  c.b = 2;
  c.c = 3;
  return c;
}

static_assert (foo ().a == 1 && foo ().b == 2 && foo ().c == 3);
