// { dg-do assemble  }
// g++ 1.37.1 bug 900403_04

// The following erroneous code causes g++ to abort.

// keywords: abort, bit-fields, zero length

struct s {
  unsigned int foo:0;		// { dg-error "" } This is the real line
  unsigned int field;
};

int main () { return 0; }
