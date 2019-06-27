/* { dg-do compile } */
/* { dg-additional-options "-O3 --save-temps" } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */

#define N 1024

signed char pix1[N], pix2[N];

int
foo (void)
{
  int i_sum = 0;
  int i;

  for (i = 0; i < N; i++)
    i_sum += __builtin_abs (pix1[i] - pix2[i]);

  return i_sum;
}

/* { dg-final { scan-assembler {\tvabdl\.s8\t} } } */
/* { dg-final { scan-assembler {\tvabal\.s8\t} } } */
/* { dg-final { scan-assembler {\tvpadal\.s16\t} } } */

/* { dg-final { scan-assembler-not {\tvmovl} } } */
/* { dg-final { scan-assembler-not {\tvsub} } } */
/* { dg-final { scan-assembler-not {\tvabs} } } */
