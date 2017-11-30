// PR c++/82219
// { dg-additional-options "-Wall -Wextra" }

struct A {
  template <typename T> T foo(T *) const { return static_cast<T>(0); }
  void bar() const { foo(&i); }
  int i;
};
