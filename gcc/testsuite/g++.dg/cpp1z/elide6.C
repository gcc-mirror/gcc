// PR c++/100368
// { dg-do compile { target c++11 } }

struct A {
  A() = default;
  A(const A&) = delete;
};

struct B { A a; }; // { dg-error "deleted" "" { target c++14_down } }

constexpr B f() { return {}; }

struct C {
  constexpr C() : b(f()) {} // { dg-error "deleted" "" { target c++14_down } }
  B b;
};
