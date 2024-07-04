// PR c++/114856

#include <initializer_list>
struct A {
  ~A();
};
struct V {
  V(std::initializer_list<A>);
};
struct data {
  V v{{}};
};
