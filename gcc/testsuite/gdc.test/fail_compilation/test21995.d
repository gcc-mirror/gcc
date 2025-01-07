/* TEST_OUTPUT:
---
fail_compilation/test21995.d(10): Error: max object size 4294967295 exceeded from adding field size 3 + alignment adjustment 1 + field offset 4294967292 when placing field in aggregate
---
*/
struct S
{
    ubyte[0x7ffffffe] a;
    ubyte[0x7ffffffe] b;
    ubyte[3] c;
}
