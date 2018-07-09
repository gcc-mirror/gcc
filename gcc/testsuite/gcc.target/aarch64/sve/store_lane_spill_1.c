/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

int cont (void);

void
f (int (*x)[3], int *a, int *b, int *c, int n)
{
  do
    for (int i = 0; i < n; ++i)
      {
	x[i][0] = a[i] + 1;
	x[i][1] = b[i] + 2;
	x[i][2] = c[i] + 3;
      }
  while (cont ());
}

/* { dg-final { scan-assembler-not {\tstr\tz[0-9]} } } */
