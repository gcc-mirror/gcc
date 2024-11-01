/* { dg-options "-O2 -fschedule-insns" } */

int
load (int *arr)
{
  return arr[527] << 1 + arr[400] << 1 + arr[401] << 1 + arr[528] << 1;
}

/* { dg-final { scan-assembler-times "ldp\tw\[0-9\]+, w\[0-9\]+, " 2 } } */

float
load_float (float *arr)
{
  return arr[404] + arr[403] + arr[400] + arr[401];
}

/* { dg-final { scan-assembler-times "ldp\ts\[0-9\]+, s\[0-9\]+, " 2 } } */

long long
load_long (long long int *arr)
{
  return arr[400] << 1 + arr[401] << 1 + arr[403] << 1 + arr[404] << 1;
}

/* { dg-final { scan-assembler-times "ldp\tx\[0-9\]+, x\[0-9\]+, " 2 } } */

double
load_double (double *arr)
{
  return arr[200] + arr[201] + arr[263] + arr[264];
}

/* { dg-final { scan-assembler-times "ldp\td\[0-9\]+, d\[0-9\]+, " 2 } } */
