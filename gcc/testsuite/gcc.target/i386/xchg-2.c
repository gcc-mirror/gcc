/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=pentium4" } */

unsigned short good(unsigned short a)
{
       return (a >> 8 | a << 8);
}

/* { dg-final { scan-assembler "\[ \t\]xchg" } } */
