/*
https://issues.dlang.org/show_bug.cgi?id=15399
REQUIRED_ARGS: -preview=dip1000
TEST_OUTPUT:
---
fail_compilation/test17977.d(19): Error: address of variable `__slList3` assigned to `elem` with longer lifetime
---
*/

@safe:
struct List {
    int* data;
    ~this();
    int* front() return;
}

void test()
{
    auto elem = List().front;
}
