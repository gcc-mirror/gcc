// PR c++/48707

struct A {
  static int a();
};

template<typename X>
struct B: A {
  static int const b;
};

template<typename X>
int const B<X>::b=B<X>::a();
