/* { dg-do compile } */
/* { dg-options "-mabs=legacy" } */

NOMIPS16 double
fabs_legacy (double d)
{
  return __builtin_fabs (d);
}

/* { dg-final { scan-assembler-not "\tabs\\.d\t" } } */
