// g++ 1.37.1 bug 900519_06

// g++ allows the type given in an invocation of operator new to be a
// reference type.

// Since pointers to reference types are illegal, the required return type
// from such an invocation (of operator new) is illegal, and thus (it seems)
// the entire call to new should be treated as being illegal.

typedef int& int_ref;

void test (int n)
{
  new int&;		// ERROR - missed
  new int_ref;		// ERROR - missed
  new int&[n];		// ERROR - missed
  new int_ref[n];	// ERROR - missed
  new int&[3];		// ERROR - missed
  new int_ref[3];	// ERROR - missed
}

int main () { return 0; }
