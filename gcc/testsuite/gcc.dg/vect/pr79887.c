/* Test for pr79887.  */
/* { dg-do compile } */
/* { dg-require-effective-target vect_condition } */
/* { dg-additional-options "-fno-trapping-math --param vect-epilogues-nomask=1" } */
/* { dg-additional-options "-mavx512ifma" { target x86_64-*-* i?86-*-* } } */

void
foo (float a[32], float b[2][32])
{
  int i;
  for (i = 0; i < 32; i++)
    a[i] = (b[0][i] > b[1][i]) ? b[0][i] : b[1][i];
}

