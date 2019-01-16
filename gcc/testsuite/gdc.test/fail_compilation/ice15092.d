/*
TEST_OUTPUT:
---
fail_compilation/ice15092.d(13): Error: struct ice15092.A.S conflicts with struct ice15092.A.S at fail_compilation/ice15092.d(12)
fail_compilation/ice15092.d(16): Error: class ice15092.A.C conflicts with class ice15092.A.C at fail_compilation/ice15092.d(15)
fail_compilation/ice15092.d(19): Error: interface ice15092.A.I conflicts with interface ice15092.A.I at fail_compilation/ice15092.d(18)
---
*/

class A
{
    struct S {}
    struct S {}

    class C {}
    class C {}

    interface I {}
    interface I {}
}
