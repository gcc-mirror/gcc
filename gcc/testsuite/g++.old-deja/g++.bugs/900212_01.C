// { dg-do assemble  }
// g++ 1.36.1 bug 900212_01

// g++ fails to flag as errors all attempts to add or subtract integer values
// from pointers-to-member values.

// Some cases are detected however.

// Cfront 2.0 passes this test.

// keywords: pointer arithmetic, member pointers

struct struct0 {
};

int struct0::*p0;
int struct0::*p1;

int (struct0::*fp0) ();
int (struct0::*fp1) ();

void global_function_0 ()
{
  p0 = p1 + 3;			// { dg-error "" } 
  p0 = p1 - 3;			// { dg-error "" } 
  p1++;				/* { dg-error "3:no post-increment" } caught by g++ */
  ++p1;				/* { dg-error "5:no pre-increment" } caught by g++ */
  p1--;				/* { dg-error "3:no post-decrement" } caught by g++ */
  --p1;				/* { dg-error "5:no pre-decrement" } caught by g++ */

  fp0 = fp1 + 3;		// { dg-error "" } 
  fp0 = fp1 - 3;		// { dg-error "" } 
  fp1++;			/* { dg-error "3:no post-increment" } */
  ++fp1;			/* { dg-error "5:no pre-increment" } */
  fp1--;			/* { dg-error "3:no post-decrement" } */
  --fp1;			/* { dg-error "5:no pre-decrement" } */
}

int main () { return 0; }
