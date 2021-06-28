/* { dg-do run } */
/* { dg-additional-options "-floop-interchange" } */

int a[6][9];
int main()
{
  a[1][3] = 8;
  for (int b = 1; b <= 5; b++)
    for (int d = 0; d <= 5; d++)
#pragma GCC unroll 0
      for (int c = 0; c <= 5; c++)
        a[b][c] = a[b][c + 2] & 216;
  for (int e = 0; e < 6; e++)
    for (int f = 0; f < 9; f++)
      if (a[e][f] != 0)
        __builtin_abort ();
  return 0;
}
