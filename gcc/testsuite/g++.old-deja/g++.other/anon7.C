// { dg-do assemble  }

struct A {
  union {
    int a;	// { dg-error "" } conflicts with previous declaration
  };
  int a;	// { dg-error "" } 
};

struct B {
  int b;	// { dg-error "" } conflicts with previous declaration
  union {
    int b;	// { dg-error "" } duplicate member
  };
};

struct C {
  union {
    int c;	// { dg-error "" } conflicts with previous declaration
  };
  union {
    int c;	// { dg-error "" } duplicate member
  };
};
