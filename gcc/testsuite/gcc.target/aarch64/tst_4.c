/* { dg-do compile } */
/* { dg-options "-O2" } */

int
f1 (long x)
{
   return ((short) x >= 0) ? x : 0;
}

/* { dg-final { scan-assembler "tst\t(x|w)\[0-9\]*.*32768\n" } } */
