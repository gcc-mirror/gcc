// PR c++/18738

namespace foo {
  typedef int my_type;
}

template<typename T>
struct A {
  typename foo::my_type bar();
};
