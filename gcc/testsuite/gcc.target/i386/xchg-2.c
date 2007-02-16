/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-Os" } */

unsigned short good(unsigned short a)
{
       return (a >> 8 | a << 8);
}

/* { dg-final { scan-assembler "xchg" } } */
