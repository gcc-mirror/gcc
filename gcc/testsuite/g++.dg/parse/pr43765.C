// PR c++/43765
// { dg-options "" }

struct SomeType
{
    int n;
    const char *values[];
};
const char *temp[] = {"607", "612", 0};

SomeType vals[] =
    {
        { 0, values : temp, },	 // { dg-error "either all initializer clauses should be designated or none of them should be" "" { target c++2a } }
        0
    };   // { dg-error "GNU-style designated initializer for an array|cannot convert" }
// (note the error above is on the wrong line)
	 // { dg-error "initialization of flexible array member in a nested context" "" { target *-*-* } .-2 }
