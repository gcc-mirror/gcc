/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-additional-options "-std=c99" } */

_Complex double y;

void
cbknu (_Complex double f)
{
  const _Complex double cone = 1.0e0;

  f = f * cone;
  y = f * cone;
}

