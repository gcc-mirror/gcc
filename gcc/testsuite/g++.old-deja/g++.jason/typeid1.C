#include <typeinfo>
#include <iostream.h>

struct foo { double f(int); };

int main() {
  double f (int);
  const std::type_info &r = typeid (f);
  cout << typeid(f).name() << endl;
  cout << typeid(foo::f).name() << endl;
}
