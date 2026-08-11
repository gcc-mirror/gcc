/* { dg-additional-options -O2 } */
/* { dg-additional-options -fdump-tree-optimized } */

int test1 (int n, int bit)
{
  int arr[16];
  int bitshift = 1 << bit;

  arr[n] = (arr[n] & bitshift) == 0 ? arr[n] : arr[n] | bitshift;

  return arr[n];
}

int test1_alt (int n, int bit)
{
  int arr[16];
  int bitshift = 1 << bit;

  arr[n] = (arr[n] & bitshift) != 0 ? arr[n] | bitshift : arr[n];

  return arr[n];
}

int test2 (int n, int bit)
{
  int arr[16];
  int bitshift = 1 << bit;

  arr[n] = ((arr[n] >> bit) & 1) == 0 ? arr[n] : arr[n] | bitshift;

  return arr[n];
}

int test2_alt (int n, int bit)
{
  int arr[16];
  int bitshift = 1 << bit;

  arr[n] = ((arr[n] >> bit) & 1) != 0 ? arr[n] | bitshift : arr[n];

  return arr[n];
}

int test3 (int n)
{
  int arr[16];
  int bits = 0xF;

  arr[n] = (arr[n] & bits) == bits ? arr[n] | bits : arr[n];

  return arr[n];
}

int test3_alt (int n)
{
  int arr[16];
  int bits = 0xF;

  arr[n] = (arr[n] & bits) != bits ? arr[n] : arr[n] | bits;

  return arr[n];
}

/* { dg-final { scan-tree-dump-times " \\| " 0 optimized } } */
/* { dg-final { scan-tree-dump-times " \& " 0 optimized } } */
/* { dg-final { scan-tree-dump-times " << " 0 optimized } } */
/* { dg-final { scan-tree-dump-times " >> " 0 optimized } } */
