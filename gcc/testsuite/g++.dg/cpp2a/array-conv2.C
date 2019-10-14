// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++2a } }

struct A {
  A();
  A(const A(&)[2]);
};

using T = A[];
using U = A[2];

// t binds directly to U{} now.  Before it bound indirectly to a temporary
// A{U{}}.  ??? But we don't do it now; see reference_binding and the 
// BRACE_ENCLOSED_INITIALIZER_P block.
A (&&t)[] = {U{}};

U u{};

T &
foo ()
{
  // This didn't compile before P0388R4: invalid initialization of non-const
  // reference of type 'A (&)[]' from an rvalue of type
  // '<brace-enclosed initializer list>'.
  return {u};
}
