// PR c++/17393
// { dg-options "-Wall" }

struct A { };

void foo()
{
  // Check that we do not complain about an unused
  // compiler-generated variable.
  A& = a; // { dg-error "token|declarator|not declared" }
}

