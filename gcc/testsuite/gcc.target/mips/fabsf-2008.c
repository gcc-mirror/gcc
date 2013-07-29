/* { dg-do compile } */
/* { dg-options "-mabs=2008" } */

NOMIPS16 float
fabsf_2008 (float f)
{
  return __builtin_fabsf (f);
}

/* { dg-final { scan-assembler "\tabs\.s\t" } } */
