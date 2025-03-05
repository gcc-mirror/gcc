/* { dg-do run } */
/* { dg-require-effective-target rv64 } */
/* { dg-require-effective-target rvv_zvl256b_ok } */
/* { dg-additional-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3 -fsigned-char -fwrapv -mrvv-vector-bits=zvl" } */

short a[4][14][14];
void
b (short d, short e, _Bool f, short g, int h, int i, char j, int k, int l,
   short m[][14], char n[], unsigned short o[][14][14], short p[])
{
  for (unsigned q = 0; q < (char) i; q++)
    for (unsigned char r = 0; r < (char) (-1748723647U * m[q][q]) - 20; r++)
      a[q][q][r] = p[q] || o[q][q][r];
}

__attribute__ ((noipa))
void
check (long long val)
{
  if (val != 0)
    __builtin_abort ();
}

long long seed;
short d;
short e;
_Bool f;
short g;
int h;
int i = 15963650;
char j;
int k;
int l;
short m[4][14];
char n[4];
unsigned short o[4][14][14];
short p[4];

int
main ()
{
  for (int q = 0; q < 4; ++q)
    {
      p[q] = 4084;
      for (int r = 0; r < 4; ++r)
	{
	  m[q][r] = 24482;
	  for (int u = 0; u < 4; ++u)
	    a[q][r][u] = 81;
	}
    }
  b (d, e, f, g, h, i, j, k, l, m, n, o, p);
  for (int q = 0; q < 4; ++q)
    for (int r = 0; r < 4; ++r)
      for (int u = 0; u < 14; ++u)
	seed ^= a[q][r][u] + (seed >> 2);

  check (seed);
}
