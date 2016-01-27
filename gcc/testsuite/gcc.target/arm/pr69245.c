/* PR target/69245 */
/* Test that pop_options restores the vfp fpu mode.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_fp } */

#pragma GCC target "fpu=vfp"

#pragma GCC push_options
#pragma GCC target "fpu=neon-vfpv4"
int a, c, d;
float b;
static int fn1 ()
{
  return 0;
}
#pragma GCC pop_options

void fn2 ()
{
  d = b * c + a;
}

/* { dg-final { scan-assembler-times "\.fpu vfp" 1 } } */
