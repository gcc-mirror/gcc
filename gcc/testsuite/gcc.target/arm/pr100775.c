/* { dg-do compile } */
/* { dg-skip-if "" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-mthumb -fzero-call-used-regs=used" } */

int
foo (int x)
{
  return x;
}
