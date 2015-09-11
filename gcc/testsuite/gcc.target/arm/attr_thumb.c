/* Check that attribute target thumb is recognized. */
/* { dg-do compile } */
/* Make sure the current multilib supports thumb.  */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-O2 -mno-restrict-it" } */
/* { dg-final { scan-assembler-not "\\.arm"  } } */
/* { dg-final { scan-assembler "\\.thumb_func" } } */

int __attribute__((target("thumb")))
foo(int a)
{
  /* { dg-final { scan-assembler "ite" { target { arm_thumb2_ok } } } } */
  return a ? 1 : 5;
}

