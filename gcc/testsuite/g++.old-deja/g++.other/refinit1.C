// { dg-do assemble  }
// Test that we don't allow multiple user-defined conversions in reference
// initialization.

struct B { };

struct A {
  A (const B&);
};

struct C {
  operator B ();
};

C c;

const A& ref (c);		// { dg-error "" } requires two UDCs
