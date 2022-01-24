/*
TEST_OUTPUT:
---
fail_compilation/fail302.d(23): Error: cannot implicitly convert expression `1` of type `int` to `Bar`
fail_compilation/fail302.d(23):        `bar = 1` is the first assignment of `bar` therefore it represents its initialization
fail_compilation/fail302.d(23):        `opAssign` methods are not used for initialization, but for subsequent assignments
---
*/

struct Bar
{
    uint num;

    Bar opAssign(uint otherNum)
    {
        num = otherNum;
        return this;
    }
}

void main()
{
    Bar bar = 1;	// disallow because construction is not assignment
    auto x = bar.num;
}
