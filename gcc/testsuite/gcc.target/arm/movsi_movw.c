/* { dg-do compile { target { arm_thumb2_ok || arm_thumb1_movt_ok } } } */
/* { dg-options "-O2" } */

int
movsi (void)
{
  return 0xF0F0;
}

/* { dg-final { scan-assembler-times "movw\tr0, #61680" 1 } } */
