/* { dg-do compile { target { arm_cortex_m && { arm_thumb2_ok || arm_thumb1_movt_ok } } } } */
/* { dg-options "-O2 -mslow-flash-data" } */

unsigned
movsi (void)
{
  return 0xF0F00000U;
}

/* { dg-final { scan-assembler-times "movt\tr0, 61680" 1 } } */
