/* TEST_OUTPUT:
---
fail_compilation/test18597.d(24): Error: field `Unaligned.p` cannot modify misaligned pointers in `@safe` code
fail_compilation/test18597.d(25): Error: field `Unaligned.p` cannot assign to misaligned pointers in `@safe` code
fail_compilation/test18597.d(26): Error: field `Unaligned.p` cannot assign to misaligned pointers in `@safe` code
---
*/

// https://issues.dlang.org/show_bug.cgi?id=18597

@safe:

align(1)
struct Unaligned
{
align(1):
    ubyte filler;
    int* p;
}

void test()
{
    Unaligned u;
    u.p = new int;
    Unaligned v = Unaligned(0, new int);
    Unaligned w = { p : new int };
}
