/* { dg-do compile } */
/* { dg-options "-Os" } */

unsigned short good(unsigned short a)
{
       return (a >> 8 | a << 8);
}

/* { dg-final { scan-assembler "xchgb" } } */
