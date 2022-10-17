/*
TEST_OUTPUT:
---
fail_compilation/ice10938.d(13): Error: no property `opts` for `this` of type `ice10938.C`
fail_compilation/ice10938.d(13):        potentially malformed `opDispatch`. Use an explicit instantiation to get a better error message
---
*/

class C
{
    this()
    {
        this.opts["opts"] = 1;
    }

    auto opDispatch(string field : "opts")()
    {
        return this.opts;  // ICE -> compile time error
    }
}

void main()
{
}
