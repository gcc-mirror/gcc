// PR c++/91364 - Implement P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do run { target c++2a } }

// Ranking of reference initialization conversions

int f(int(&)[]) { return 1; }	    // (1)
int f(int(&)[1]) { return 2; }	    // (2)

int h(int(*)[]) { return 1; }	    // (a)
int h(int(*)[1]) { return 2; }	    // (b)

// From P0388R4:
// (2) and (b) should clearly be better than (1) and (a), respectively,
// as the former overloads are more restricted. 
// (a) should be worse than (b), which is implied by (a) necessitating
// a qualification conversion in that case.

int
main ()
{
  int arr[1];
  if (f(arr) != 2)
    __builtin_abort ();
  if (h(&arr) != 2)
    __builtin_abort ();
}
