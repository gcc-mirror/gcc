// PR c++/48945
// { dg-options -std=c++0x }

struct A {
  static constexpr bool is();
};

constexpr bool A::is() { return true; }
