// Test that we don't allow multiple user-defined conversions in reference
// initialization.
// Build don't link:

struct B { };

struct A {
  A (const B&);
};

struct C {
  operator B ();
};

C c;

const A& ref (c);		// ERROR - requires two UDCs
