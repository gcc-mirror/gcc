/* { dg-do compile } */
/* { dg-options "-O3" } */

#pragma GCC target "+nosve+nodotprod"

#define N 1024

unsigned char pix1[N], pix2[N];

int foo (void)
{
  int i_sum = 0;
  int i;

  for (i = 0; i < N; i++)
    i_sum += __builtin_abs (pix1[i] - pix2[i]);

  return i_sum;
}

/* { dg-final { scan-assembler-not {\tushll\t} } } */
/* { dg-final { scan-assembler-not {\tushll2\t} } } */
/* { dg-final { scan-assembler-not {\tusubl\t} } } */
/* { dg-final { scan-assembler-not {\tusubl2\t} } } */
/* { dg-final { scan-assembler-not {\tabs\t} } } */

/* { dg-final { scan-assembler {\tuabdl2\t} } } */
/* { dg-final { scan-assembler {\tuabal\t} } } */
/* { dg-final { scan-assembler {\tuadalp\t} } } */
