/*
TEST_OUTPUT:
---
fail_compilation/fail12236.d(16): Error: forward reference to inferred return type of function `f1`
fail_compilation/fail12236.d(16):        while evaluating `pragma(msg, f1.mangleof)`
fail_compilation/fail12236.d(21): Error: forward reference to inferred return type of function `f2`
fail_compilation/fail12236.d(21):        while evaluating `pragma(msg, f2(T)(T).mangleof)`
fail_compilation/fail12236.d(27): Error: template instance `fail12236.f2!int` error instantiating
fail_compilation/fail12236.d(31): Error: forward reference to inferred return type of function `__lambda1`
fail_compilation/fail12236.d(31):        while evaluating `pragma(msg, __lambda1(__T1)(a).mangleof)`
---
*/

auto f1(int)
{
    pragma(msg, f1.mangleof);  // forward reference error
}

auto f2(T)(T)
{
    pragma(msg, f2.mangleof);  // error <- weird output: "v"
}

void main()
{
    f1(1);
    f2(1);

    (a) {
        int x;
        pragma(msg, __traits(parent, x).mangleof);
    } (1);
}
