/* PR target/67745
   Verify alignment when attribute target is used.  */
/* { dg-do compile } */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-skip-if "" arm_cortex_m } */
/* { dg-options "-Os -mthumb" }  */

/* Check that arm code is always 4 bytes aligned.  */
void  __attribute__ ((target ("arm")))
c(void)
{
}

/* { dg-final { scan-assembler-not ".align\[ \t]1" } } */
