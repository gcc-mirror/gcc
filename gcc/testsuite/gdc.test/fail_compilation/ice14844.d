/*
TEST_OUTPUT:
---
fail_compilation/ice14844.d(20): Error: template `opDispatch(string name)` has no members
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
