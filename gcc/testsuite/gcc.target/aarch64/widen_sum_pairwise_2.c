/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+dotprod -mautovec-preference=asimd-only --param vect-epilogues-nomask=0" } */

/* With dot product a 4x widening sum stays a single [SU]DOT, while a
   sum into 64-bit elements uses the pairwise widening instructions.  */

int
sum_u8_i (const unsigned char *a, long n)
{
  int s = 0;
  for (long i = 0; i < n; i++)
    s += a[i];
  return s;
}

long
sum_u8_l (const unsigned char *a, long n)
{
  long s = 0;
  for (long i = 0; i < n; i++)
    s += a[i];
  return s;
}

/* { dg-final { scan-assembler-times {\tudot\tv[0-9]+\.4s, v[0-9]+\.16b, v[0-9]+\.16b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuaddlp\tv[0-9]+\.8h, v[0-9]+\.16b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuaddlp\tv[0-9]+\.4s, v[0-9]+\.8h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuadalp\tv[0-9]+\.2d, v[0-9]+\.4s\n} 1 } } */
/* { dg-final { scan-assembler-not {\tuaddw2?\t} } } */
