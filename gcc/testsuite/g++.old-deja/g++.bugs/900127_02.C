// { dg-do assemble  }
// g++ 1.36.1 bug 900127_02

// g++ (mostly) keeps separate name spaces for the declarations of data
// objects and functions.

// This means that a single name may be declared as both a data object and
// a function within a given scope.

// This fact allows programmers to write code which is not portable to the
// Cfront translator (which keeps a single namespace for these entities).

// This can also lead to ambiguity when the & (address-of) operator is used.

// Cfront 2.0 passes this test.

// keywords: name spaces, overloading

int global0;			// { dg-message "" } 
int global0 ();			// { dg-error "" } 

int global1 ();			// { dg-message "" } xref for below
int global1;			// { dg-error "" } caught

struct struct_0 {
  int class_local ();		// { dg-message "" }
  int class_local;		// { dg-error "" } 
};

struct struct_1 {
  int class_local;		// { dg-message "" }
  int class_local ();		// { dg-error "" } 
};

void function_0 ()
{
	int function_0_local;	// { dg-message "" } 
	extern int function_0_local ();	// { dg-error "" } 
}

void function_1 ()
{
	int function_1_local ();        // { dg-message "" } 
	extern int function_1_local;	// { dg-error "" } 
}

int main () { return 0; }
