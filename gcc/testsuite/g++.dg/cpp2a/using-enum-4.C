// Test for suggestion to try 'using enum'.
// { dg-do compile { target c++20 } }

struct A
{
  enum E { e };
};

struct B
{
  using A::E;			// { dg-error "" }
  // { dg-message "using enum" "" { target *-*-* } .-1 }
};
