/* { dg-do run } */
/* { dg-options "-O3" } */

unsigned char b, f;
short d[1][8][1], *g = &d[0][3][0];

void __attribute__((noinline)) foo ()
{
  int k[256] = { 0, 0, 0, 4, 0, 0 };
  for (int c = 252; c >= 0; c--)
    {
      b = f;
      *g = k[c + 3];
      k[c + 1] = 0;
    }
  for (int i = 0; i < 8; i++)
    if (d[0][i][0] != 0)
      __builtin_abort ();
}

void __attribute__((noinline)) bar ()
{
  int k[256] = { 0, 0, 0, 4, 0, 0 };
  k[255] = 4;
  for (int c = 0; c <=252; c++)
    {
      b = f;
      *g = k[c + 3];
      k[c + 1] = 0;
    }
  for (int i = 0; i < 8; i++)
    if ((i == 3 && d[0][i][0] != 4) || (i != 3 && d[0][i][0] != 0))
      __builtin_abort ();
}

int main ()
{
  foo ();
  bar ();
  return 0;
}
