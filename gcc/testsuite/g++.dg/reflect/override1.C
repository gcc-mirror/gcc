// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// CWG 3117 - Overriding by a consteval virtual function

using info = decltype(^^::);

struct B {
  virtual void foo();
};

struct D1 : B {
  consteval virtual void foo() override { } // { dg-error "overriding" }
};

struct D2 : B {
  info i;
  consteval virtual void foo() override { }
};

struct D3 : B {
  virtual void foo() override { }
};

struct B2 {
  consteval virtual void foo() { }
};

struct D4 : B2 {
  consteval virtual void foo() override { }
};

struct D5 : B2 {
  virtual void foo() override { } // { dg-error "overriding" }
};

struct D6 : B2 {
  info i;
  virtual void foo() override { } // { dg-error "consteval-only type|overriding" }
};
