template <int J>
struct A {
};

struct B {
  template <int I>
  struct C : public A<I> {};

  typedef double I;
};
