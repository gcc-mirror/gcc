/*
REQUIRED_ARGS: -m64
PERMUTE_ARGS:
---
fail_compilation/staticarrayoverflow.d(21): Error: static array S[1879048192] size overflowed to 7516192768000
fail_compilation/staticarrayoverflow.d(21): Error: variable staticarrayoverflow.y size overflow
fail_compilation/staticarrayoverflow.d(22): Error: variable staticarrayoverflow.z size of x1000ae0 exceeds max allowed size 0x100_0000
fail_compilation/staticarrayoverflow.d(23): Error: static array S[8070450532247928832] size overflowed to 0
fail_compilation/staticarrayoverflow.d(23): Error: variable staticarrayoverflow.a size overflow
---
*/



struct S
{
    int[1000] x;
}

S[0x7000_0000] y;
S[0x100_0000/(4*1000 - 1)] z;
S[0x7000_0000_0000_0000] a;

