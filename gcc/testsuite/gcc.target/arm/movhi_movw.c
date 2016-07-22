/* { dg-do compile { target { arm_thumb2 || arm_thumb1_movt_ok } } } */
/* { dg-options "-O2" } */

short
movsi (void)
{
  return (short) 0x7070;
}

/* { dg-final { scan-assembler-times "movw\tr0, #28784" 1 } } */
