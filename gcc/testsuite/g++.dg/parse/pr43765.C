// PR c++/43765
// { dg-options "" }

struct SomeType
{
    const char *values[];
};
const char *temp[] = {"607", "612", 0};

SomeType vals[] =
    {
        { values : temp, },
        0
    };          // { dg-error "invalid" }
