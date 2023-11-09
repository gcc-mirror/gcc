/* { dg-do compile } */
/* { dg-options "-mnan=2008" } */

NOMIPS16 double
fabs_2008 (double d)
{
  return __builtin_fabs (d);
}

/* { dg-final { scan-assembler "\tabs\\.d\t" } } */
