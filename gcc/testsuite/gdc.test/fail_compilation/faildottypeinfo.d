/*
TEST_OUTPUT:
---
fail_compilation/faildottypeinfo.d(11): Error: no property `typeinfo` for type `int`
fail_compilation/faildottypeinfo.d(12): Error: no property `typeinfo` for type `object.Object`
---
*/

void main()
{
    auto x = 0.typeinfo;
    auto y = Object.typeinfo;
}
