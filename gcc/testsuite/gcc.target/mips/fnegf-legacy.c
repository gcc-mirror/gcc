/* { dg-do compile } */
/* { dg-options "-mabs=legacy" } */

NOMIPS16 float
fnegf_legacy (float f)
{
  return -f;
}

/* { dg-final { scan-assembler-not "\tneg\.s\t" } } */
