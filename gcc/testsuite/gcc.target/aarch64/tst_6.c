/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (long x)
{
   return ((short) x != 0) ? x : 1;
}

/* { dg-final { scan-assembler "tst\t(x|w)\[0-9\]+,\[ \t\]*65535" } } */
