// { dg-do assemble  }
// g++ 1.36.1 bug 900221_01

// Ref: 3.2
//
// Section 3.2 of the C++ 2.0 Reference Manual says:
//
//	"Names of formal arguments for a function are treated as if they
//	were declared in the outermost block of that function"
//
// g++ does not enforce this treatment.

// Cfront 2.0 passes this test.

// keywords: scope, formal parameters

void function (int arg1)
{
  int arg1;		// { dg-error "" } redeclaration of arg1
}

int main () { return 0; }
