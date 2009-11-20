// { dg-do assemble  }
// { dg-options "-fshow-column" }

struct A { // { dg-error "" } forward declaration
  friend struct B : A {		// { dg-error "invalid use of incomplete type 'struct A" }
    int x;
  };	// { dg-error "class definition may not be declared a friend" ""  { target *-*-* } { 5 } }
  int y;
};
