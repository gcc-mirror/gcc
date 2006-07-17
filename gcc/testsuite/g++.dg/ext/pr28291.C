
// Test to make sure we do not ICE on this invalid program.

// { dg-do compile }
// { dg-options "" }

struct A
{
  static int i;
  int j;
};

A a = { i:0 }; // { dg-error "non-static data member" }
