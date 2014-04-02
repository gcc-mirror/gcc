// DR 990
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct A {
  A(std::initializer_list<int>);  // #1
};
struct B {
  A a;
};

void f (B);
int main()
{
  B{};
  f({});
  B b0 = { };
  B b1 { };    // OK, uses #1
  B b2 { 1 };  // { dg-error "could not convert" }
}
