// { dg-do assemble  }

struct A { // { dg-error "" } forward declaration
  friend struct B : A {		// { dg-error "" } 
    int x;
  };	// { dg-error "" } class definition cannot be a friend
  int y;
};
