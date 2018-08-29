/* { dg-options "-O2" } */

void
store (int *arr, int x, int y, int z)
{
  arr[400] = x;
  arr[401] = y;

  arr[500] = z;
  arr[501] = x;
}

/* { dg-final { scan-assembler-times "stp\tw\[0-9\]+, w\[0-9\]+, " 2 } } */

void
store_float (float *arr, float x, float y)
{
  arr[404] = x;
  arr[403] = y;

  arr[400] = x;
  arr[401] = y;
}

/* { dg-final { scan-assembler-times "stp\ts\[0-9\]+, s\[0-9\]+, " 2 } } */

void
store_long (long long int *arr, long long int x, long long int y)
{
  arr[400] = x;
  arr[401] = y;

  arr[403] = y;
  arr[404] = x;
}

/* { dg-final { scan-assembler-times "stp\tx\[0-9\]+, x\[0-9\]+, " 2 } } */

void
store_double (double *arr, double x, double y)
{
  arr[200] = x;
  arr[201] = y;

  arr[263] = y;
  arr[264] = x;
}

/* { dg-final { scan-assembler-times "stp\td\[0-9\]+, d\[0-9\]+, " 2 } } */
