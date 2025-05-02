// { dg-do compile { target c++23 } }

struct A { ~A(); };

struct B {
  constexpr B() {
    A a;
    for (int i = 0; i < 10; i++) { }
  } // { dg-error "call to non-'constexpr' function 'A::~A..'" }
};

constexpr bool f() {
  B b; // { dg-error "B::B..' called in a constant expression" }
  return true;
}

static_assert(f()); // { dg-error "non-constant" }
