/* { dg-do compile } */
/* { dg-require-effective-target arm_cortex_m } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mcpu=*" } { "-mcpu=cortex-m4" "-mcpu=cortex-m7" } } */
/* { dg-skip-if "do not override -mfloat-abi" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-skip-if "-mslow-flash-data and -mword-relocations incompatible" { *-*-* } { "-mword-relocations" } } */
/* { dg-options "-march=armv7e-m+fp -mfloat-abi=hard -mthumb -mslow-flash-data" } */

/* From PR71607 */

float b;
void fn1 ();

float
fn2 ()
{
  return 1.1f;
}

void
fn3 ()
{
  float a[2];
  a[1] = b;
  fn1 (a);
}
