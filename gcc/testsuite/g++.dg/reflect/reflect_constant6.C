// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_constant.  The glvalue-to-prvalue conversion may have
// side-effects as part of the constant expression evaluation that the call to
// reflect_constant is part of.

#include <meta>

using namespace std::meta;

struct B {
  int *const p;
  consteval B(int *p) : p(p) {}
};

consteval int g() {
  int x = 42;
  B b(&x);
  reflect_constant (b);
  return x;
}
static_assert(g() == 43); // { dg-error "uncaught exception|non-constant" }

struct A {
  int *const p;
  consteval A(int *p) : p(p) {}
  consteval A(const A &oth) : p(nullptr) {
    if (oth.p) {
      ++*oth.p;
    }
  }
};

consteval int f() {
  int x = 42;
  A a(&x);
  reflect_constant (a);
  return x;
}

// FIXME Clang++ accepts this, but we throw: we think that the temporary
// argument to reflect_constant can't be a NTTP.
// As reflect_constant accept by value, the parameter of reflect_constant(a)
// is copy-initialized with a, which set p to nullptr, i.e. we no longer 
// have pointer to stack variable.
static_assert(f() == 43); // { dg-error "uncaught exception|non-constant" }
