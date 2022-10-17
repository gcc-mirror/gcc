/*
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test20881.d(20): Error: scope variable `this` may not be returned
fail_compilation/test20881.d(27): Error: address of variable `s` assigned to `global` with longer lifetime
fail_compilation/test20881.d(28): Error: address of variable `s` assigned to `global` with longer lifetime
fail_compilation/test20881.d(29): Error: address of variable `s` assigned to `global` with longer lifetime
---
*/
@safe:

// https://issues.dlang.org/show_bug.cgi?id=20881
struct S
{
    int* ptr;

    auto borrowA() return /*scope inferred*/ { return ptr; }
    int* borrowB() return { return ptr; }
    int* borrowC() scope return { return ptr; }
}

void main()
{
    static int* global;
    S s;
    global = s.borrowA;
    global = s.borrowB;
    global = s.borrowC;
}
