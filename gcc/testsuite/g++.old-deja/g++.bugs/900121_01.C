// { dg-do assemble  }
// g++ 1.36.1 bug 900121_01

// The following file causes g++ 1.36.1 (and 1.36.2) to abort.

// Cfront 2.0 passes this test.

// keywords: abort, incomplete types, reference types, formal parameters

struct s0;              // { dg-message "" } forward declaration

void function (struct s0 &arg1, struct s0 &arg2)
{
  arg1 = arg2;		// { dg-error "" } causes abort
}

int main () { return 0; }
