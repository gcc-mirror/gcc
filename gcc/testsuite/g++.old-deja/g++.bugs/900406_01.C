// { dg-do assemble  }
// g++ 1.37.1 bug 900406_01

// The following code causes g++ to segfault.

// cfront 2.0 passes this test.

// keywords: segfault, operator new, array types, array bounds

void function0 ()
{
  new int[];		// { dg-error "" } causes segfault
}

void function1 ()
{
  new int (*)[];	// { dg-error "" } no size specified 
}

int main () { return 0; }
