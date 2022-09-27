/*
TEST_OUTPUT:
---
fail_compilation/failoffset.d(12): Error: no property `offset` for `b` of type `int`
fail_compilation/failoffset.d(12):        while evaluating: `static assert(b.offset == 4)`
---
*/

void main()
{
    struct S { int a, b; }
    static assert(S.b.offset == 4);
}
