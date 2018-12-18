/* { dg-do compile } */
/* { dg-require-effective-target arm_cortex_m } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mcpu=*" } { "-mcpu=cortex-m4" "-mcpu=cortex-m7" } } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mword-relocations" } } */
/* { dg-options "-march=armv7e-m+fp -mfloat-abi=hard -O2 -mthumb -mslow-flash-data" } */

float f (float);

const float max = 0.01f;

int
g (float in)
{
  if (f (in) + f (in) < max)
    return 0;
  return 1;
}

double foo (void)
{
  return 0xF1EC7A5239123AF;
}

double bar (void)
{
  return 0.0f;
}
