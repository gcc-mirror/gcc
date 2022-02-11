/*
REQUIRED_ARGS: -m64
TEST_OUTPUT:
---
fail_compilation/staticarrayoverflow.d(23): Error: static array `S[1879048192]` size overflowed to 7516192768000
fail_compilation/staticarrayoverflow.d(23): Error: variable `staticarrayoverflow.y` size overflow
fail_compilation/staticarrayoverflow.d(25): Error: static array `S[8070450532247928832]` size overflowed to 8070450532247928832
fail_compilation/staticarrayoverflow.d(25): Error: variable `staticarrayoverflow.a` size overflow
fail_compilation/staticarrayoverflow.d(26): Error: static array `S[0][18446744073709551615LU]` size overflowed to 18446744073709551615
fail_compilation/staticarrayoverflow.d(26): Error: variable `staticarrayoverflow.b` size overflow
fail_compilation/staticarrayoverflow.d(27): Error: static array `S[0][4294967295]` size overflowed to 4294967295
fail_compilation/staticarrayoverflow.d(27): Error: variable `staticarrayoverflow.c` size overflow
---
*/



struct S
{
    int[1000] x;
}

S[0x7000_0000] y;
S[0x100_0000/(4*1000 - 1)] z;
S[0x7000_0000_0000_0000] a;
S[0][-1] b;
S[0][uint.max] c;
