/*
TEST_OUTPUT:
---
fail_compilation/ice16657.d(9): Error: function `ice16657.RefCounted.refCountedPayload` has no `return` statement, but is expected to return a value of type `inout(RefCounted)`
---
*/
struct RefCounted
{
    inout(RefCounted) refCountedPayload() inout { }
    alias refCountedPayload this;
}

struct Store
{
    RefCounted p;
}
