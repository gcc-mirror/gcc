/* { dg-do compile } */
/* { dg-options "-mabs=2008" } */

NOMIPS16 double
fneg_2008 (double d)
{
  return -d;
}

/* { dg-final { scan-assembler "\tneg\\.d\t" } } */
