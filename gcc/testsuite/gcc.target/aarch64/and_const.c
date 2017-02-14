/* { dg-do compile } */
/* { dg-options "-O2" } */

int f2 (int x)
{
   x &= 0x0ffffff8;

   x &= 0xff001fff;

   return x;
}

/* { dg-final { scan-assembler-times "and\t" 2 } } */
/* { dg-final { scan-assembler-not "movk\t" } } */
