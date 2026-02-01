// https://github.com/dlang/dmd/issues/21504

// Test case where `@safe` triggers attribute inference of all nested
// declarations, and the NOGCVisitor for `new` expressions encounters a self
// referencing GC action.

void test21504() @nogc @safe
{
    static struct Nest
    {
        this(int i)
        {
            static assert(__traits(compiles, new Nest(i)));
        }
    }
    auto s = Nest(1);
}
