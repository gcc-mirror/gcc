extern void abort (void);

static int inv_J(int a[][2])
{
  int i, j;
  int det = 0.0;
   for (j=0; j<2; ++j)
     det += a[j][0] + a[j][1];
  return det;
}

int foo()
{
  int mat[2][2];
  mat[0][0] = 1;
  mat[0][1] = 2;
  mat[1][0] = 4;
  mat[1][1] = 8;
  return inv_J(mat);
}

int main()
{
  if (foo () != 15)
    abort ();
  return 0;
}

