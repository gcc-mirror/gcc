/* { dg-do compile } */
/* { dg-options "-O3 -ftree-vectorize -fno-vect-cost-model" } */
float arr_f[100][100];
float
f9 (void)
{

  int i;
  float sum = 0;
  for (i = 0; i < 100; i++)
    sum += arr_f[i][0] * arr_f[0][i];
  return sum;

}


int arr[100][100];
int
f10 (void)
{

  int i;
  int sum = 0;
  for (i = 0; i < 100; i++)
    sum += arr[i][0] * arr[0][i];
  return sum;

}

double arr_d[100][100];
double
f11 (void)
{
  int i;
  double sum = 0;
  for (i = 0; i < 100; i++)
    sum += arr_d[i][0] * arr_d[0][i];
  return sum;
}

char arr_c[100][100];
char
f12 (void)
{
  int i;
  char sum = 0;
  for (i = 0; i < 100; i++)
    sum += arr_c[i][0] * arr_c[0][i];
  return sum;
}


/* { dg-final { scan-assembler-not "sp" } } */
