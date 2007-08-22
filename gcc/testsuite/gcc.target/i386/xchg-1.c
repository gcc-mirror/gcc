/* { dg-do compile } */
/* { dg-options "-O2 -march=k8" } */

unsigned short good(unsigned short a)
{
       return (a >> 8 | a << 8);
}

/* { dg-final { scan-assembler "rol" } } */
