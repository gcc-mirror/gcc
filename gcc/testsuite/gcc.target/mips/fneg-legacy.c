/* { dg-do compile } */
/* { dg-options "-mabs=legacy" } */

NOMIPS16 double
fneg_legacy (double d)
{
  return -d;
}

/* { dg-final { scan-assembler-not "\tneg\\.d\t" } } */
