// { dg-do run  }
// g++ bug 900406_02

// g++ fails to correctly parse some type specifications within casts.

// This results in incorrect errors being issued.

// These errors are not issued for identical code by either cfront or
// by gcc.

// cfront 2.0 passes this test.

// keywords: syntax, array types, casts

int (*ipp)[];
int (**ippp)[];

int function ()
{
  ipp = (int (*)[]) 0;			// OK
  ippp = (int (**)[]) 0;		// { dg-bogus "" }  (syntax)
  return 0;
}

int main () { return 0; }

