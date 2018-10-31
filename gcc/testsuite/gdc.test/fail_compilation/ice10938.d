/*
TEST_OUTPUT:
---
fail_compilation/ice10938.d(12): Error: no property 'opts' for type 'ice10938.C'
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
