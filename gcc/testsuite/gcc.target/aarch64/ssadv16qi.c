/* { dg-do compile } */
/* { dg-options "-O3" } */

#pragma GCC target "+nosve+nodotprod"

#define N 1024

signed char pix1[N], pix2[N];

int foo (void)
{
  int i_sum = 0;
  int i;

  for (i = 0; i < N; i++)
    i_sum += __builtin_abs (pix1[i] - pix2[i]);

  return i_sum;
}

/* { dg-final { scan-assembler-not {\tsshll\t} } } */
/* { dg-final { scan-assembler-not {\tsshll2\t} } } */
/* { dg-final { scan-assembler-not {\tssubl\t} } } */
/* { dg-final { scan-assembler-not {\tssubl2\t} } } */
/* { dg-final { scan-assembler-not {\tabs\t} } } */

/* { dg-final { scan-assembler {\tsabdl2\t} } } */
/* { dg-final { scan-assembler {\tsabal\t} } } */
/* { dg-final { scan-assembler {\tsadalp\t} } } */
