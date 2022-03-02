/* { dg-do run } */
/* { dg-additional-options "-floop-unroll-and-jam" } */

int a[3], b, c;
static int e()
{
  if (!c) {
    for (b = 0; b < 3; b++)
      for (c = 0; c < 3; c++)
        a[c] ^= 1;
    return -1;
  }
  return 0;
}
int main()
{
  e();
  if (a[1] != 1)
    __builtin_abort();
  return 0;
}
