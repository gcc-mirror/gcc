/* { dg-do compile } */
/* { dg-options "-mabs=2008" } */

NOMIPS16 float
fnegf_2008 (float f)
{
  return -f;
}

/* { dg-final { scan-assembler "\tneg\\.s\t" } } */
