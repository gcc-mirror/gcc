/* { dg-do compile } */
/* { dg-options "-O2" } */

short foo ()
{
  int t = 5;
  short r = __builtin_bfin_ones(t);
  return r;
}

/* { dg-final { scan-assembler-not "ONES" } } */
