// https://issues.dlang.org/show_bug.cgi?id=22686

/*
TEST_OUTPUT:
---
fail_compilation/test22686.d(15): Error: `this` is only defined in non-static member functions, not `create`
---
*/

struct S
{
    int[] data;
    static auto create()
    {
        auto self = &this;
        return {
            assert(data.length);
            return self;
        };
    }
}
