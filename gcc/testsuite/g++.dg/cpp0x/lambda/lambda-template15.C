// PR c++/82069
// { dg-do compile { target c++11 } }

struct A {
  void foo(int *);
};
struct B : A {
  template <typename> void bar(int *p1) {
    [&] { foo(p1); };
  }
};
