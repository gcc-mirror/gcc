// { dg-do assemble  }
// g++ 1.37.1 bug 900401_01

// The following erroneous code causes g++ to abort.

// Cfront 2.0 passes this test.

// keywords: abort, bit-fields, arrays

typedef unsigned nibble_array[4];

struct whole {
  nibble_array nibbles:16;	// { dg-error "" } 
};

int main () { return 0; }
