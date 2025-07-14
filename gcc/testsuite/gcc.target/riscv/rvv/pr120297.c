/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fwhole-program" } */

unsigned a;
short c;
char d;
unsigned long e;
_Bool f[10][10];
unsigned g[10];
long long ak;
char i = 7;
long long t[10];
short x[10][10][10][10];
short y[10][10][10][10];

void
h (char i, long long t[], short x[][10][10][10], short y[][10][10][10],
   _Bool aa)
{
  for (int j = 2; j < 8; j += 2)
    {
      for (short k = 0; k < 10; k++)
	{
	  for (int l = 3; l < 8; l += 2)
	    a = x[1][j][k][l];
	  c = x[c][1][1][c];
	}
      for (int k = 0; k < 10; k++)
	{
	  f[2][k] |= (_Bool) t[c];
	  g[c] = t[c + 1];
	  d += y[j][1][k][k];
	  e = e > i ? e : i;
	}
    }
}

int
main ()
{
  t[c] = 1;
  h (i, t, x, y, a);
  for (int j = 0; j < 10; ++j)
    for (int k = 0; k < 10; ++k)
      ak ^= f[j][k] + 238516665 + (ak >> 2);
  ak ^= g[c] + 238516665 + (ak >> 2);
  if (ak != 234635118ull)
    __builtin_abort ();
}
