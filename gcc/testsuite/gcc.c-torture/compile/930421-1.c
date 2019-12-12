/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

double q(double);

f (int **x, int *r, int *s, int a, int b, int c, int d)
{
  int i, j, k, m, e, f, g, z[1024], y[2];

  e = g = 0;
  for (i = 0; i < a; i++)
    for (j = 0; j < b; j++)
      if (x[i][j])
	for (k = 0; k < c; k++)
	  {
	    f = q(1.5) + q(2.5);
	    if (g < y[f])
	      g = e;
	  }
  for (m = 0; m < 1; m++)
    z[0] = m*2*d/3.0 - d;
}
