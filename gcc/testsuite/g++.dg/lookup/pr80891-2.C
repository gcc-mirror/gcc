// PR c++/80891 part 1
// instantiation-time ADL for swap needs to copy a previous lookup
// node, but gets confused.

void swap();

namespace boost {
  void swap();
}

using namespace boost;

template <typename T>
void reversible_container_test ()
{
  using namespace boost;
  T a;
  swap (a);
}

namespace boost {
  struct A {};
  template <typename T> void swap(T);
}

void test_ptr_vector()
{
  reversible_container_test<A>;
}
