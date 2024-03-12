/**
TEST_OUTPUT:
---
fail_compilation\hexstring.d(16): Error: cannot implicitly convert expression `"123F"` of type `string` to `immutable(ubyte[])`
fail_compilation\hexstring.d(17): Error: cannot implicitly convert expression `"\x12?"c` of type `string` to `immutable(ubyte[])`
fail_compilation\hexstring.d(18): Error: cannot implicitly convert expression `"\x12?"` of type `string` to `immutable(ubyte[])`
fail_compilation\hexstring.d(15): Error: cannot implicitly convert expression `"\x12?"` of type `string` to `ubyte[]`
---
*/
immutable ubyte[] s0 = x"123F";
static assert(s0[0] == 0x12);
static assert(s0[1] == 0x3F);
immutable byte[] s1 = x"123F";

ubyte[] f1 = x"123F";
immutable ubyte[] f2 = "123F";
immutable ubyte[] f3 = x"123F"c;
immutable ubyte[] f4 = cast(string) x"123F";
