// { dg-do assemble  }
// g++ 1.36.1 bug 900212_02

// g++ fails to flag as errors attempts to take the difference of two values
// of some compatible pointer-to-member type.

// Cfront 2.0 passes this test.

// keywords: pointer arithmetic, subtraction, member pointers

struct struct0 {
};

int struct0::*p0;
int struct0::*p1;

int (struct0::*fp0) ();
int (struct0::*fp1) ();

int result;

void global_function_0 ()
{
  result = (p0 - p1);		// { dg-error "" } 
  result = (fp0 - fp1);		// { dg-error "" } 
}

int main () { return 0; }
