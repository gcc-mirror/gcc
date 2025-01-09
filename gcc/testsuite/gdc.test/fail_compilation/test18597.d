/* TEST_OUTPUT:
---
fail_compilation/test18597.d(24): Error: modifying misaligned pointers through field `Unaligned.p` is not allowed in a `@safe` function
fail_compilation/test18597.d(25): Error: field `Unaligned.p` assigning to misaligned pointers is not allowed in a `@safe` function
fail_compilation/test18597.d(26): Error: field `Unaligned.p` assigning to misaligned pointers is not allowed in a `@safe` function
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
