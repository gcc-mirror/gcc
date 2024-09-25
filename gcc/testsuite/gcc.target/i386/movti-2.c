/* { dg-do compile { target int128 } } */
/* { dg-options "-Os -mavx" } */
__int128 m;

void foo()
{
    m &= ((__int128)0x0123456789abcdefULL<<64) | 0x0123456789abcdefULL;
}

/* { dg-final { scan-assembler-times "movabsq" 1 } } */
/* { dg-final { scan-assembler-not "vmovq" } } */
/* { dg-final { scan-assembler-not "vpunpcklqdq" } } */
