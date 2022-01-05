// PR c++/102191
// { dg-do compile { target c++20 } }

struct X {
  struct A {
    constexpr ~A() noexcept(false) { }
  };

  constexpr A operator()(auto...) { return {}; }
};

void f() { []() consteval { X{}(); }(); }
