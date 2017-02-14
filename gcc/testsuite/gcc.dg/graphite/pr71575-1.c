/* { dg-do compile } */
/* { dg-options "-O2 -floop-nest-optimize" } */

void w(int x, double *y)
{
  int i, j;
  double a;
  double c[32];

  for (i = 0; i < x; i++) {
      for (j = 0; j < x - i; j++) {
	  c[j] = y[i];
      }
      y[i] = a;
      a += c[0] + y[i];
  }
}

void v(int x, double *y)
{
  w(x, y);
}
