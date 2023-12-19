/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mavx" } */

__int128 m;

void bar()
{
    m = ((__int128)0x0123456789abcdefULL<<64) | 0x0123456789abcdefULL;
}

/* { dg-final { scan-assembler "vmovdqa" } } */
/* { dg-final { scan-assembler-not "vpextrq" } } */
