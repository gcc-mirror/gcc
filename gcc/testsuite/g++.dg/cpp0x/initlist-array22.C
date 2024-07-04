// PR c++/111286
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wno-unused }

struct A {
  A() noexcept {}
};

void foo() {
  using T = const A (&)[1];
  T{};
}
