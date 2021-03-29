// PR tree-optimization/99777

template <typename T>
inline const T &
min (const T &a, const T &b)
{
  if (b < a)
    return b;
  return a;
}

template <typename T>
inline const T &
max (const T &a, const T &b)
{
  if (a < b)
    return b;
  return a;
}

extern int o, a, c;
long h;
unsigned long long e;
signed char d;
extern short p[][7][5][30];

void
test (long long b, short f[][17][25][22][20])
{
  for (char i = 0; i < 7; i += 3)
    for (unsigned char l = e; l < 5; l += 2)
      {
	if (max (0LL, min (7LL, b)))
	  for (bool j = 0; j < 1; j = b)
	    {
	      for (unsigned k = d; k < 20; k++)
		h = f[0][i][l][b][k];
	      for (int m = 0; m < 5; m++)
		p[c][i][l][m] = 0;
	    }
	for (int n = 0; n < 4; n += a)
	  o = n;
      }
}
