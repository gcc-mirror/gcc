/* PR target/127020 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4.1 -mtune=cascadelake -mstv -mno-stackrealign" } */

extern __int128 m0, m1, m2, m3;

void
foo (unsigned long x)
{
  m0 = x;
  m1 = x;
  m2 = x;
  m3 = x;
}

/* { dg-final { scan-assembler-times "(movaps|vmovdqa)" 4 } } */
