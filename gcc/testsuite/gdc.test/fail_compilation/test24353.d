// https://issues.dlang.org/show_bug.cgi?id=24353

/**
TEST_OUTPUT:
---
fail_compilation/test24353.d(23): Error: mutable method `test24353.S.opApply` is not callable using a `const` object
fail_compilation/test24353.d(14):        Consider adding `const` or `inout` here
---
*/


struct S
{
    int opApply(int delegate(int) dg)
    {
        return 0;
    }
}

void example()
{
    const S s;
    foreach (e; s) {} // Error expected here
}
