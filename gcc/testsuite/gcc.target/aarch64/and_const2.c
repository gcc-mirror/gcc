/* { dg-do compile } */
/* { dg-options "-O2" } */

long long f2 (long x)
{
   x &= 0x0ffffffffffffff8LL;

   x &= 0xff001fffLL;

   return x;
}

/* { dg-final { scan-assembler-times "and\t" 2 } } */
/* { dg-final { scan-assembler-not "movk\t" } } */
