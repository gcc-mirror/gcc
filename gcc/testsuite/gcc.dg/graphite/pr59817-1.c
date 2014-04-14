/* { dg-do compile } */
/* { dg-options "-O2 -floop-interchange" } */

int kd;

void
n2(void)
{
  static int so;
  static short int i5;
  int wj;
  int *il;
  int *nk = &so;
  for (wj = 0; wj < 2; ++wj)
    *nk = ((i5 += *il) || kd );
}
