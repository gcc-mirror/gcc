/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign" } */

__int128 m0,m1,m2,m3;
void foo(unsigned int x)
{
    __int128 m = x;
    m0 = m;
    m1 = m;
    m2 = m;
    m3 = m;
}

/* { dg-final { scan-assembler-times "movaps" 4 } } */
