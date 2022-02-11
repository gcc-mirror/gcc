/*
TEST_OUTPUT:
---
fail_compilation/ice16035.d(18): Error: forward reference to inferred return type of function call `this.a[0].toString()`
fail_compilation/ice16035.d(13): Error: template instance `ice16035.Value.get!string` error instantiating
---
*/

struct Value
{
    auto toString() inout
    {
        get!string;
    }

    T get(T)()
    {
        a[0].toString();
    }

    const(Value)* a;
}
