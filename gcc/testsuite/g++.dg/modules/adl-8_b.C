// { dg-additional-options "-fmodules -std=c++26" }

export module X;
export import M;

export template <typename T>
void test3(T t) {
  test2(t);
}

namespace other {
  export struct S {};
  void foo(S) {}
}
