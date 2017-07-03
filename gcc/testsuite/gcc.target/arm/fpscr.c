/* Test the fpscr builtins.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_vfp_ok } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */
/* { dg-options "-mfpu=vfp -mfloat-abi=softfp" } */

void
test_fpscr ()
{
  volatile unsigned int status = __builtin_arm_get_fpscr ();
  __builtin_arm_set_fpscr (status);
}

/* { dg-final { scan-assembler "mrc\tp10, 7, r\[0-9\]+, cr1, cr0, 0" } } */
/* { dg-final { scan-assembler "mcr\tp10, 7, r\[0-9\]+, cr1, cr0, 0" } } */
