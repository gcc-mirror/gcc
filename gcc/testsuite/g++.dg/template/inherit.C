// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>
// { dg-do compile }

template<typename T>
struct X { void f() { } };

struct Z : X<int> { };

int main()
{
  Z z;
  z.X::f();
}
