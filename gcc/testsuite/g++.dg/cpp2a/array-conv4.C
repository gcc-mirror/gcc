// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++2a } }

// Ranking of reference initialization conversions

void f(int(&)[]) {}	    // (1)
//void f(int(&)[1]) { }	    // (2)
void f(int*) { }	    // (3)

//void f2(int(&)[]) { }	    // (1)
void f2(int(&)[1]) { }	    // (2)
void f2(int*) { }	    // (3)

// From P0388R4:
// (3) should be equal to (1) (as it is to (2))
// Check that we get "ambiguous overload" errors.

void
doit ()
{
  int arr[1];
  f(arr); // { dg-error "ambiguous" }
  f2(arr); // { dg-error "ambiguous" }
}
