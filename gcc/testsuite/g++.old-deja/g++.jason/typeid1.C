#include <typeinfo>
#include <iostream.h>

struct foo { double f(int); };

main() {
  double f (int);
  const type_info &r = typeid (f);
  cout << typeid(f).name() << endl;
  cout << typeid(foo::f).name() << endl;
}
