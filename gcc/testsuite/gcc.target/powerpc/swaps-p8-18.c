/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O3" } */
/* { dg-final { scan-assembler-not "xxpermdi" } } */

/* This is a test for a specific convert-splat permute removal.  */

void compute (float*, float*, float*, int, int);
double test (void);
double gorp;

int main (void)
{
  float X[10000], Y[256], Z[2000];
  int i;
  for (i = 0; i < 2500; i++)
    compute (X, Y, Z, 256, 2000);
  gorp = test ();
}

void compute(float *X, float *Y, float *Z, int m, int n)
{
  int i, j;
  float w, *x, *y;

  for (i = 0; i < n; i++)
    {
      w = 0.0;
      x = X++;
      y = Y;
      for (j = 0; j < m; j++)
	w += (*x++) * (*y++);
      Z[i] = w;
    }
}
