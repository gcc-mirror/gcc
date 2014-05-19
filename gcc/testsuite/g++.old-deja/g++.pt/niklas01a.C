// { dg-do assemble  }
// { dg-options "-fshow-column" }

struct A { // { dg-message "" } forward declaration
  friend struct B : A {		// { dg-error "invalid use of incomplete type 'struct A" "invalid" }
    int x;
  };	// { dg-error "class definition may not be declared a friend" "may not"  { target *-*-* } { 5 } }
  int y;
};
