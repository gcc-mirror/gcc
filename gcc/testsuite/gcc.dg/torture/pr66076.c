/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-options "-mno-prefer-avx128 -march=bdver4" { target i?86-*-* x86_64-*-* } } */

void
f0a (char *result, char *arg1, char *arg4, char temp_6)
{
  int idx = 0;
  for (idx = 0; idx < 416; idx += 1)
    result[idx] = (arg1[idx] + arg4[idx]) * temp_6;
}
