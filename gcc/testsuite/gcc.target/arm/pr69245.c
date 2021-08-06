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
 int fn1 ()
{
  return 0;
}
#pragma GCC pop_options

void fn2 ()
{
  d = b * c + a;
}

/* Because we don't know the exact command-line options used to invoke the test
   we cannot expect these tests to match exactly once.  But they must appear at
   least once.  */
/* { dg-final { scan-assembler "\.fpu\\s+vfp\n" } } */
/* { dg-final { scan-assembler "\.fpu\\s+neon-vfpv4\n" } } */
