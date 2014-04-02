// PR c++/48945
// { dg-do compile { target c++11 } }

struct A {
  static constexpr bool is();
  static constexpr bool is_not();
};

constexpr bool A::is() { return true; }
constexpr bool A::is_not() const { return true; } // { dg-error "static" }
