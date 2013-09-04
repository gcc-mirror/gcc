/* { dg-do compile } */
/* { dg-options "-mabs=legacy" } */

NOMIPS16 float
fabsf_legacy (float f)
{
  return __builtin_fabsf (f);
}

/* { dg-final { scan-assembler-not "\tabs\\.s\t" } } */
