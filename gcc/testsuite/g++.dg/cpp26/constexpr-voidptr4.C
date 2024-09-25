// PR c++/116741
// { dg-do compile { target c++26 } }

struct S {
  int foo;    // { dg-message "pointed-to object" }
};

struct S2 {
  int foo;    // { dg-message "pointed-to object" }
};

struct X {
  S2 s;
};

constexpr float f1() {
  S s;
  void* p = &s.foo;
  return *static_cast<float*>(p); // { dg-error "not allowed in a constant expression" }
}

constexpr float f2() {
  X x;
  void* p = &x.s.foo;
  return *static_cast<float*>(p); // { dg-error "not allowed in a constant expression" }
}

constexpr auto x1 = f1();
constexpr auto x2 = f2();
