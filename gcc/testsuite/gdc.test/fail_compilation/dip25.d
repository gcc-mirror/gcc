/*
REQUIRED_ARGS: -de
TEST_OUTPUT:
---
fail_compilation/dip25.d(17): Deprecation: returning `this.buffer[]` escapes a reference to parameter `this`
fail_compilation/dip25.d(17):        perhaps annotate the function with `return`
fail_compilation/dip25.d(22): Error: returning `identity(x)` escapes a reference to local variable `x`
fail_compilation/dip25.d(23): Deprecation: returning `identity(x)` escapes a reference to parameter `x`
fail_compilation/dip25.d(23):        perhaps annotate the parameter with `return`
---
*/
struct Data
{
    char[256] buffer;
    @property const(char)[] filename() const pure nothrow
    {
        return buffer[];
    }
}

ref int identity(return ref int x) { return x; }
ref int fun(return int x) { return identity(x); }
ref int fun2(ref int x) { return identity(x); }

void main()
{
    Data d;
    const f = d.filename;
}
