// PR c++/91483
// { dg-do compile { target c++14 } }

struct X {
  int const& var;
};

struct A {
  A *ap = this;
};

constexpr A
foo ()
{
  return {};
}

void
test ()
{
  constexpr int i = 42; // { dg-message "may differ on each invocation" }

  constexpr X x{i}; // { dg-error "not a constant expression" }
  // { dg-message "reference to .i. is not a constant expression" "" { target *-*-* } .-1 }
  constexpr const int *p = &i; // { dg-error "not a constant expression" }
  // { dg-message "pointer to .i. is not a constant expression" "" { target *-*-* } .-1 }

  constexpr A a = foo (); // { dg-error "not a constant expression" }
  // { dg-message "pointer to .a. is not a constant expression|may differ" "" { target *-*-* } .-1 }

  constexpr const int *q = __builtin_launder (&i); // { dg-error "not a constant expression" }
  // { dg-message "pointer to .i. is not a constant expression" "" { target *-*-* } .-1 }
}
