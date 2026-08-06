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
	{ 0, values : temp, },	 // { dg-warning "either all initializer clauses should be designated or none of them should be" "" { target { c++20 && c++26_down } } }
        0
    };
// (note the error below is on the wrong line)
// { dg-error "initialization of flexible array member in a nested context" "" { target c++17_down } .-2 }
// { dg-error "last non-designated initializer clause does not appertain to a base class subobject" "" { target c++20 } .-3 }
