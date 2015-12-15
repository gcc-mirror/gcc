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
        { 0, values : temp, },
        0
    };   // { dg-error "GNU-style designated initializer for an array|cannot convert" }
// (note the error above is on the wrong line)
 
