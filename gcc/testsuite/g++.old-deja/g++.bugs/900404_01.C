// { dg-do assemble  }
// g++ 1.37.1 bug 900404_01

// g++ allows string initializers for known-length character arrays to be
// one character longer (counting the terminating null) than the actual
// length of the array to be initialized.

// The C++ Reference Manual (section 8.4.2) expressly prohibits this.

// Cfront 2.0 passes this test.

// keywords: arrays, initialization, array bounds

char cv[4] = "asdf";		// { dg-error "" } missed

int main () { return 0; }
