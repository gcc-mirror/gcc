/*
TEST_OUTPUT:
---
fail_compilation/ice10938.d(14): Error: no property `opts` for `this` of type `ice10938.C`
fail_compilation/ice10938.d(19): Error: forward reference to inferred return type of function call `this.opDispatch()`
fail_compilation/ice10938.d(14): Error: template instance `ice10938.C.opDispatch!"opts"` error instantiating
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
