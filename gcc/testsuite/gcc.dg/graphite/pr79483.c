/* { dg-do compile } */
/* { dg-options "-O2 -fgraphite-identity" } */

int *a;
extern int b[];
int c;
void d ()
{
  double e[2][3] = {0.0, 0.0, 1.0};
  for (int f = 0; f < 2; ++f)
    for (int g = 0; g < 6; ++g)
      b[0] = a[g] * e[f][2];
  c = b[0];
}
