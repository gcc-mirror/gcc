/* { dg-do compile } */

void
vec_slp_int16_t (short int *restrict a, short int *restrict b, int n)
{
  short int x0 = b[0];
  short int x1 = b[1];
  short int x2 = b[2];
  short int x3 = b[3];
  for (int i = 0; i < n; ++i)
  {
    x0 += a[i * 4];
    x1 += a[i * 4 + 1];
    x2 += a[i * 4 + 2];
    x3 += a[i * 4 + 3];
  }
  b[0] = x0;
  b[1] = x1;
  b[2] = x2;
  b[3] = x3;
}
