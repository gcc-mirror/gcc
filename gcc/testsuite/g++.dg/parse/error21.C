// PR c++/17393
// { dg-options "-Wall -fshow-column" }

struct A { };

void foo()
{
  // Check that we do not complain about an unused
  // compiler-generated variable.
  A& = a; // { dg-error "6: error: expected unqualified-id before '=' token|8: error: 'a' was not declared in this scope" }
}

