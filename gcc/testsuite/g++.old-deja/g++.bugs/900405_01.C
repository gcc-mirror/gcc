// g++ 1.37.1 bug 900405_01

// The C++ Reference Manual says (in section 5.4) "Types may not be defined
// in casts."

// g++ fails to flag errors for cases where an attempt is made to define
// a struct, class, union, or enum type within a cast.

// keywords: casts, type definitions, tagged types

void f ()
{
  (enum e { red, green } *) 0;		// ERROR - type defined in cast
  (struct s { int member; } *) 0;	// ERROR - type defined in cast
  (union u { int member; } * ) 0;	// ERROR - type defined in cast
  (class c { int member; } *) 0;	// ERROR - type defined in cast
}

int main () { return 0; }
