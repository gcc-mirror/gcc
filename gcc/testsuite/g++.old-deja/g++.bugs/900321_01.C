// { dg-do assemble  }
// g++ 1.37.1 bug 900321_01

// cfront flags ERRORs on each of the lines indicated below. g++ does not
// flag either ERRORs or warnings.

// Although I cannot find where in the current C++ Reference Manual this
// topic is covered, I am sure that these statements should get ERRORs in
// a "strongly typed" language.

// Cfront 2.0 passes this test.

// keywords: array types, array bound, pointers

int (*ptr_to_array_of_ints)[];
int (*ptr_to_array_of_3_ints) [3];
int (*ptr_to_array_of_5_ints) [5];

void function_0 ()
{
  // we miss the first two because typeck.c (comp_array_types) deems
  // it okay if one of the sizes is null
  ptr_to_array_of_ints = ptr_to_array_of_3_ints;	// { dg-error "" } 
  ptr_to_array_of_3_ints = ptr_to_array_of_ints;	// { dg-error "" } 

  ptr_to_array_of_3_ints = ptr_to_array_of_5_ints;	// { dg-error "" } 
  ptr_to_array_of_5_ints = ptr_to_array_of_3_ints;	// { dg-error "" } 
}

int main () { return 0; }
