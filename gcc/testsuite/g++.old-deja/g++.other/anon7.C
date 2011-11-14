// { dg-do assemble  }

struct A {
  union {
    int a;	// { dg-message "" } conflicts with previous declaration
  };
  int a;	// { dg-error "" } 
};

struct B {
  int b;	// { dg-message "" } conflicts with previous declaration
  union {
    int b;	// { dg-error "" } duplicate member
  };
};

struct C {
  union {
    int c;	// { dg-message "" } conflicts with previous declaration
  };
  union {
    int c;	// { dg-error "" } duplicate member
  };
};
