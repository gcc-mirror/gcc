/* { dg-do assemble } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */

/* Each function uses the correspoding 'CLASS' in
   Marco CHECK (aarch64_simd_valid_immediate).  */

void
orr_0 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] |= 0xab;
}

void
orr_1 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] |= 0x0000cd00;
}

void
orr_2 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] |= 0x00ef0000;
}

void
orr_3 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] |= 0x12000000;
}

void
orr_4 (short *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] |= 0x00340034;
}

void
orr_5 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] |= 0x56005600;
}

/* { dg-final { scan-assembler "orr\\tv\[0-9\]+.4s, #171" } } */
/* { dg-final { scan-assembler "orr\\tv\[0-9\]+.4s, #205, lsl #8" } } */
/* { dg-final { scan-assembler "orr\\tv\[0-9\]+.4s, #239, lsl #16" } } */
/* { dg-final { scan-assembler "orr\\tv\[0-9\]+.4s, #18, lsl #24" } } */
/* { dg-final { scan-assembler "orr\\tv\[0-9\]+.8h, #52" } } */
/* { dg-final { scan-assembler "orr\\tv\[0-9\]+.8h, #86, lsl #8" } } */
