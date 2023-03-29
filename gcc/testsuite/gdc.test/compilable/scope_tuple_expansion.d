// REQUIRED_ARGS: -preview=dip1000

// Reduced from `std.systime`.
// Tuple expansion can trip up scope checking with errors like:
// Error: scope variable `__tup4` assigned to `found` with longer lifetime

struct Tuple(T...)
{
    T t;
    alias t this;
}

Tuple!(int*, int) find(return scope int* x) @safe
{
    assert(0);
}

void fromISOExtString(scope int* str) @safe
{
    int* found = str.find()[0];
}
