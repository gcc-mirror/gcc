/*
TEST_OUTPUT:
---
fail_compilation/ice14844.d(21): Error: in expression `__traits(allMembers, opDispatch)` template `opDispatch(string name)` has no members
fail_compilation/ice14844.d(21):        `opDispatch(string name)` must evaluate to either a module, a struct, an union, a class, an interface or a template instantiation
---
*/

struct Typedef
{
    template opDispatch(string name)
    {
        static if (true)
        {
        }
    }
}

void runUnitTestsImpl()
{
    foreach (x; __traits(allMembers, Typedef.opDispatch))
    {
    }
}
