// PR c++/92752
// { dg-do compile }
// { dg-additional-options "-Wignored-qualifiers" }

struct X;

template<class T>
struct Wrap {
  T data;
  Wrap() : data() {}
};

typedef int (X::*type)();
Wrap<const type> x;
#if __cpp_initializer_lists
const type t{};
#endif
