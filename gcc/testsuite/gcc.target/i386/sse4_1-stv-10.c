/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign" } */

__int128 m0,m1,m2,m3;
void foo(__int128 m)
{
    m0 = m;
    m1 = m;
    m2 = m;
    m3 = m;
}

/* { dg-final { scan-assembler-times "movaps" 4 } } */
