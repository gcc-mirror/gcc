/* { dg-do compile } */
/* { dg-options "-O2 -mabi=ilp32" } */

long long
load_long (long long int *arr)
{
  return arr[400] << 1 + arr[401] << 1 + arr[403] << 1 + arr[404] << 1;
}

/* { dg-final { scan-assembler-times "ldp\tx\[0-9\]+, x\[0-9\]+, " 2 } } */

int
load (int *arr)
{
  return arr[527] << 1 + arr[400] << 1 + arr[401] << 1 + arr[528] << 1;
}

/* { dg-final { scan-assembler-times "ldp\tw\[0-9\]+, w\[0-9\]+, " 2 } } */
