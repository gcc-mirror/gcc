/* Check -mflip-thumb. */
/* { dg-do compile } */
/* Make sure the current multilib supports thumb.  */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-skip-if "" arm_cortex_m } */
/* { dg-options "-O2 -mflip-thumb -mno-restrict-it" } */
/* { dg-final { scan-assembler "\\.arm" } } */
/* { dg-final { scan-assembler-times "\\.thumb_func" 1} } */

int 
foo(int a)
{
  return a ? 1 : 5;
}

int 
bar(int a)
{
  return a ? 1 : 5;
}

/* { dg-final { scan-assembler-times "ite" 1 { target { arm_thumb2_ok } } } } */





