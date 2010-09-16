/* { dg-do compile } */
/* { dg-options "-O2 -m8bit-idiv" } */

int
foo (int x, int y)
{
   return x % y;
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-times "idivl" 1 } } */
