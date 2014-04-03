/* { dg-options "-O2 -floop-interchange" } */

int **db6 = 0;

void
k26(void)
{
  static int geb = 0;
  int *a22 = &geb;
  int **l30 = &a22;
  int *c4b;
  int ndf;
  for (ndf = 0; ndf <= 1; ++ndf)
    *c4b = (db6 == l30) && (*a22)--;
}

