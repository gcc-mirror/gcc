// { dg-do assemble  }

struct A {
  int x;
  int y ();
  int z ();
  int foo (int j);
};

int A::foo (int q) { return q + (this->*(x ? &A::y : &A::z)) (); }	// { dg-bogus "" } 
