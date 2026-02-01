// https://issues.dlang.org/show_bug.cgi?id=24353

/*
REQUIRED_ARGS: -verrors=context
TEST_OUTPUT:
---
fail_compilation/test24353.d(37): Error: mutable method `test24353.S.opApply` is not callable using a `const(S)` foreach aggregate
    foreach (e; s) {} // mod error
                ^
fail_compilation/test24353.d(28):        Consider adding a method type qualifier here
    int opApply(int delegate(int) dg)
        ^
fail_compilation/test24353.d(40): Error:  shared const method `test24353.S2.opApply` is not callable using a `const(S2)` foreach aggregate
    foreach (i, e; s2) {} // mod error
                   ^
fail_compilation/test24353.d(47):        Consider adding a method type qualifier here
    int opApply(int delegate(int, int) dg) const shared;
        ^
fail_compilation/test24353.d(42): Error: cannot uniquely infer `foreach` argument types
    foreach (i, e; const S3()) {} // cannot infer
    ^
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
    foreach (e; s) {} // mod error

    const S2 s2;
    foreach (i, e; s2) {} // mod error

    foreach (i, e; const S3()) {} // cannot infer
}

struct S2
{
    int opApply(int delegate(int, int) dg) const shared;
}

struct S3
{
    int opApply(int delegate(int) dg);
    int opApply(int delegate(int, int) dg);
}
