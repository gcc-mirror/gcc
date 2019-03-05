// REQUIRED_ARGS: -o-
// PERMUTE_ARGS:

struct Vector(T, int N)
{
    void opDispatch(string, U)(U)
    {
    }

    void baz(string, U)(U)
    {
    }
}

static assert(!is(typeof(Vector!(int, 2)._isMatrix)));
static assert(!is(typeof(Vector!(int, 2).baz!"_isMatrix")));
