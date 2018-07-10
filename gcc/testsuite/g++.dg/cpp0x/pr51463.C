// PR c++/51463
// { dg-do compile { target c++11 } }

struct A
{
  static virtual int i = 0;	// { dg-error "10:member .i. cannot be declared both .virtual. and .static." }
  // { dg-error "declared as" "" { target *-*-* } .-1 }
};
