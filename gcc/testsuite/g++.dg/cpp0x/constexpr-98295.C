// PR c++/98295
// { dg-do compile { target c++11 } }

struct A { constexpr A(); };

void f() {
  A b[2][3];
  [b] { };
}

constexpr A::A() {}
