/*
REQUIRED_ARGS:
TEST_OUTPUT:
---
fail_compilation/dip25.d(17): Error: escaping a reference to parameter `this` by returning `this.buffer[]` is not allowed in a `@safe` function
fail_compilation/dip25.d(15):        perhaps annotate the function with `return`
fail_compilation/dip25.d(22): Error: returning `identity(x)` escapes a reference to parameter `x`
fail_compilation/dip25.d(23): Error: escaping a reference to parameter `x` by returning `identity(x)` is not allowed in a `@safe` function
fail_compilation/dip25.d(23):        perhaps annotate the parameter with `return`
---
*/
struct Data
{
    char[256] buffer;
    @property const(char)[] filename() const pure nothrow @safe
    {
        return buffer[];
    }
}

ref int identity(return ref int x) @safe { return x; }
ref int fun(return int x) @safe { return identity(x); }
ref int fun2(ref int x) @safe { return identity(x); }

void main()
{
    Data d;
    const f = d.filename;
}
