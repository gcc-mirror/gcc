// { dg-do compile }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
#include <typeinfo>
#include <iostream>

struct foo { double f(int); };

int main() {
  double f (int);
  const std::type_info &r = typeid (f);
  std::cout << typeid(f).name() << std::endl;
  std::cout << typeid(foo::f).name() << std::endl; /* { dg-error "" } */
}
