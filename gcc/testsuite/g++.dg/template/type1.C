// Test for helpful error messages on invalid nested-name-specifiers.

struct A {
  template <class T> struct B { static int c; };
};

int A::B::c;			// { dg-error "parameters" }
int A::C::d;			// { dg-error "no type" }
