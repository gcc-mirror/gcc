// PR c++/42567
// { dg-options "-std=c++0x" }

template<typename B>
struct A {
  template<typename C>
  void fn(C c) {
    auto& key = *c;
  }
};
