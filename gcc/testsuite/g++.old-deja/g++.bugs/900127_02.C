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

int global0;			// ERROR - 
int global0 ();			// ERROR - 

int global1 ();			// ERROR - xref for below
int global1;			// ERROR - caught

struct struct_0 {
  int class_local ();		// ERROR - 
  int class_local;		// ERROR - 
};

struct struct_1 {
  int class_local;		// ERROR - 
  int class_local ();		// ERROR - 
};

void function_0 ()
{
	int function_0_local;	// ERROR - 
	extern int function_0_local ();	// ERROR - 
}

void function_1 ()
{
	int function_1_local ();        // ERROR - 
	extern int function_1_local;	// ERROR - 
}

int main () { return 0; }
