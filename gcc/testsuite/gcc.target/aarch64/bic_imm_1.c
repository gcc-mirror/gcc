/* { dg-do assemble } */
/* { dg-options "-O2 --save-temps -ftree-vectorize" } */

#pragma GCC target "+nosve"

/* Each function uses the correspoding 'CLASS' in
   Marco CHECK (aarch64_simd_valid_immediate).  */

void
bic_6 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] &= ~(0xab);
}

void
bic_7 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] &= ~(0xcd00);
}

void
bic_8 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] &= ~(0xef0000);
}

void
bic_9 (int *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] &= ~(0x12000000);
}

void
bic_10 (short *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] &= ~(0x34);
}


void
bic_11 (short *a)
{
  for (int i = 0; i < 1024; i++)
    a[i] &= ~(0x5600);
}


/* { dg-final { scan-assembler "bic\\tv\[0-9\]+.4s, #171" } } */
/* { dg-final { scan-assembler "bic\\tv\[0-9\]+.4s, #205, lsl #8" } } */
/* { dg-final { scan-assembler "bic\\tv\[0-9\]+.4s, #239, lsl #16" } } */
/* { dg-final { scan-assembler "bic\\tv\[0-9\]+.4s, #18, lsl #24" } } */
/* { dg-final { scan-assembler "bic\\tv\[0-9\]+.8h, #52" } } */
/* { dg-final { scan-assembler "bic\\tv\[0-9\]+.8h, #86, lsl #8" } } */
