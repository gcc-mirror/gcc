// { dg-do assemble  }
// g++ 1.37.1 bug 900330_01
//
// As indicated by the example at the end of the section 3.5.3 of the ANSI
// C standard, when a type qualifier (i.e. "const" or "volatile") is applied
// to an array type, the effect should be as if the element type had been
// qualified with the given qualifier.
//
// This rule applies to C++ also.
//
// In section 7.1.6 of the C++ Reference Manual it says "Each element of a
// const array is const..."
//
// It appears however that when a name already exists for a given array type
// (i.e. a typedef name) and when that name is qualified by a type qualifier,
// (i.e. "const" or "volatile"), gcc & g++ may act as if the qualifier applied
// to the named (array) type rather that to the elements of that type.
//
// The result is that (even with the -ansi and -pedantic options) g++
// generates no errors or warnings for the lines indicated (even though it
// should).
//
// Due to the incorrect associations, gcc & g++ will also issue inappropriate
// warnings in some cases (as illustrated below).

// keywords: type qualifiers, arrays

typedef const int const_int;
typedef const_int array_of_const_int[3];
array_of_const_int *ptr_to_array_of_consts;

typedef int array_of_int[3];
typedef const array_of_int const_array_of_int;
const_array_of_int *ptr_to_const_array;

void function_0 ()
{
  ptr_to_array_of_consts = ptr_to_const_array;	/* gets bogus warning */
  ptr_to_const_array = ptr_to_array_of_consts;	/* gets bogus warning */
}

/* The following example is taken from ANSI 3.5.3 */

typedef int A[2][3];
const A a = {{4, 5, 6}, {7, 8, 9}};
int *pi;

void function_1 ()
{
  pi = a[0];	// { dg-error "" } a[0] has type "const int *"
}

int main () { return 0; }
