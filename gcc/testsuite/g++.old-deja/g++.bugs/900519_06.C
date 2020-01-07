// { dg-do assemble  }
// g++ 1.37.1 bug 900519_06

// g++ allows the type given in an invocation of operator new to be a
// reference type.

// Since pointers to reference types are illegal, the required return type
// from such an invocation (of operator new) is illegal, and thus (it seems)
// the entire call to new should be treated as being illegal.

typedef int& int_ref;

void test (int n)
{
  new int&;		// { dg-error "3:new cannot be applied to a reference type" } missed
  new int_ref;		// { dg-error "3:new cannot be applied to a reference type" } missed
  new int&[n];		// { dg-error "" } missed
  new int_ref[n];	// { dg-error "3:new cannot be applied to a reference type" } missed
  new int&[3];		// { dg-error "" } missed
  new int_ref[3];	// { dg-error "3:new cannot be applied to a reference type" } missed
}

int main () { return 0; }
