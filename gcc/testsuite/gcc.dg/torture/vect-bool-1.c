/* { dg-do compile } */

int a[64];
long b[64];

void foo (void)
{
  for (int i = 0; i < 64; ++i)
    {
      _Bool x = a[i] < 10;
      a[i] = x;
      b[i] = x;
    }
}
