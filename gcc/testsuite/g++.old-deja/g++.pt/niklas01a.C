// { dg-do assemble  }

struct A { // { dg-error "" } forward declaration
  friend struct B : A {		// { dg-error "" } 
    int x;
  };
  int y;
};
