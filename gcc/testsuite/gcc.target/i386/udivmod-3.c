/* { dg-do compile } */
/* { dg-options "-O2 -m8bit-idiv" } */

unsigned int
foo (unsigned int x, unsigned int y)
{
   return x % y;
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-times "divl" 1 } } */
