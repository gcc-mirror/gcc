/* Verify that the rte delay slot is not stuffed with register pop insns
   which touch the banked registers r0..r7 on SH3* and SH4* targets.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m3*" "-m4*" } }  */
/* { dg-final { scan-assembler-times "nop" 1 } } */

int __attribute__ ((interrupt_handler))
test00 (int a, int b, int c, int d)
{
  return a + b;
}
