// g++ 1.37.1 bug 900402_01

// The following erroneous code causes g++ to abort.

// Cfront 2.0 passes this test.

// keywords: abort, bit-fields, function types

typedef void (func_type) ();

struct s {
  func_type f:32;	// ERROR - bitified with function type
};

int main () { return 0; }
