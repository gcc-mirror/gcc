// Build don't link:

struct A {
  union {
    int a;	// ERROR - conflicts with previous declaration
  };
  int a;	// ERROR - 
};

struct B {
  int b;	// ERROR - conflicts with previous declaration
  union {
    int b;	// ERROR - duplicate member
  };		// ERROR - declaration of
};

struct C {
  union {
    int c;	// ERROR - conflicts with previous declaration
  };
  union {
    int c;	// ERROR - duplicate member
  };		// ERROR - declaration of
};
