// { dg-do assemble  }
// g++ 1.37.1 bug 900403_01

// g++ allows casts to be treated as lvalues (even when the -pedantic
// option is used).  Neither the C++ Reference Manual nor cfront 2.0
// allow this.  (gcc gives warnings for such usage with -pedantic).

// Cfront 2.0 passes this test.

// keywords: lvalues, casts

int i, j;

void f ()
{
  (int) i = j;		// { dg-error "" } 
  ((int) i) = j;	// { dg-error "" } 
}

int main () { return 0; }
