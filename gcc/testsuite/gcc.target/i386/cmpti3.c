/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__int128 x;
int foo()
{
    __int128 t = 0x1234567890abcdefLL;
    return x == t;
}

/* { dg-final { scan-assembler-times "movabsq" 1 } } */
/* { dg-final { scan-assembler-times "xorq" 1 } } */
/* { dg-final { scan-assembler-not "xorl" } } */
