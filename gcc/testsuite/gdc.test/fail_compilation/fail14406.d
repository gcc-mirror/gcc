/*
TEST_OUTPUT:
---
fail_compilation/fail14406.d-mixin-20(20): Error: variable `fail14406.CFrop.bar_obj` cannot be further field because it will change the determined CFrop size
fail_compilation/fail14406.d-mixin-25(25): Error: variable `fail14406.IFrop.bar_obj` field not allowed in interface
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
