/* { dg-additional-options -O2 } */
/* { dg-additional-options -fdump-tree-optimized } */

int bitset1 (int n, int bit)
{
  int arr[16];
  int bitshift = 1 << bit;

  if ((arr[n] & bitshift) == 0)
    arr[n] |= bitshift;

  return arr[n];
}

int bitset1_alt (int n, int bit)
{
  int arr[16];
  int bitshift = 1 << bit;

  arr[n] = (arr[n] & bitshift) != 0 ? arr[n] : arr[n] | bitshift;

  return arr[n];
}

int bitset1b (int n, int bit)
{
  int arr[16];
  int bitshift = 1 << bit;

  if ((bitshift & arr[n]) == 0)
    arr[n] |= bitshift;

  return arr[n];
}

int bitset1b_alt (int n, int bit)
{
  int arr[16];
  int bitshift = 1 << bit;

  arr[n] = (bitshift & arr[n]) != 0 ? arr[n] : bitshift | arr[n];

  return arr[n];
}

int bitset2 (int n)
{
  int arr[16];
  int bits = 0xF;

  if ((arr[n] & bits) != bits)
    arr[n] |= bits;

  return arr[n];
}

int bitset2_alt (int n)
{
  int arr[16];
  int bits = 0xF;

  arr[n] = (arr[n] & bits) == bits ?  arr[n]: arr[n] | bits;

  return arr[n];
}

int bitset2b (int n)
{
  int arr[16];
  int bits = 0xF;

  if ((bits & arr[n]) != bits)
    arr[n] |= bits;

  return arr[n];
}

int bitset2b_alt (int n)
{
  int arr[16];
  int bits = 0xF;

  arr[n] = (bits & arr[n]) == bits ?  arr[n]: bits | arr[n];

  return arr[n];
}

/* A negative test to ensure we're not optimizing something
   we shouldn't. */
int bitset_nosimplify (int n)
{
  int arr[16];

  int bits = 0xF;

  /* We can't make the bit_ior unconditional here because the
     cond matches for multiple values aside from '0xF' in
     which the value wouldn't be touched, e.g. for arr[n] = 0x1
     "arr[n] & bits" is also non-zero and the value wouldn't be
     changed.  */
  if ((arr[n] & bits) == 0)
    arr[n] |= bits;

  return arr[n];
}

/* { dg-final { scan-tree-dump-times "goto" 2 optimized } } */
