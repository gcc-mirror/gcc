/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign" } */

unsigned __int128 a;
unsigned __int128 b;

void foo()
{
  a = b << 16;
}

void bar()
{
  a = b >> 16;
}

/* { dg-final { scan-assembler "pslldq" } } */
/* { dg-final { scan-assembler "psrldq" } } */
