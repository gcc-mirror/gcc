/*
REQUIRED_ARGS: -preview=systemVariables
TEST_OUTPUT:
---
fail_compilation/cast_system.d(31): Error: cast from `int[1]` to `UniqueInt` is not allowed in a `@safe` function
fail_compilation/cast_system.d(31):        Target element type has unsafe bit patterns
---
*/

struct UniqueInt
{
    @system int n;

    this(int n) @safe
    {
        this.n = n;
    }

    @disable this(ref inout UniqueInt) inout;

    ~this() @trusted
    {
        this.n = 0;
    }
}

void main() @safe
{
    auto s = cast(UniqueInt) 7; // OK, calls ctor
    int[1] a;
    s = cast(UniqueInt) a; // doesn't call ctor
}

void ok() @safe
{
    auto b = cast(bool) 7; // OK
}
