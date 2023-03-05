/*
TEST_OUTPUT:
---
fail_compilation/fail14406.d-mixin-20(20): Error: cannot declare field `bar_obj` because it will change the determined size of `CFrop`
fail_compilation/fail14406.d-mixin-25(25): Error: field `bar_obj` not allowed in interface
---
*/

class Foo {}

string strMixin(T)()
{
    static if (T.tupleof.length) {}
    return "Bar bar_obj;
    static class Bar {  Foo foo; }";
}

class CFrop
{
    mixin(strMixin!(typeof(this)));
}

interface IFrop
{
    mixin(strMixin!(typeof(this)));
}
