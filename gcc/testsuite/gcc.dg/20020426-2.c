/* PR optimization/6475
   Distilled from zlib sources.  */
/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -frename-registers -fomit-frame-pointer -fPIC -mtune=i686" { target i?86-*-* } } */

typedef struct
{
  union
  {
    struct
    {
      unsigned char a3;
      unsigned char a4;
    } a2;
    unsigned int a5;
  } a0;
  unsigned int a1;
} A;

static int
foo (unsigned int *b, unsigned int n, unsigned int s, const unsigned int *d,
     const unsigned int *e, A **t, unsigned int *m, A *hp, unsigned int *hn,
     unsigned int *v)
{
  unsigned int a, c[15 + 1], f;
  int g, h;
  unsigned int i, j, k;
  int l;
  unsigned int ee;
  unsigned int *p;
  A *q, r, *u[15];
  int w;
  unsigned int x[15 + 1], *xx;
  int y;
  unsigned int z;
  p = c;
  *p++ = 0; *p++ = 0; *p++ = 0; *p++ = 0;
  *p++ = 0; *p++ = 0; *p++ = 0; *p++ = 0;
  *p++ = 0; *p++ = 0; *p++ = 0; *p++ = 0;
  *p++ = 0; *p++ = 0; *p++ = 0; *p++ = 0;
  p = b;
  i = n;
  do
    c[*p++]++;
  while (--i);
  if (c[0] == n)
    {
      *t = (A *) 0;
      *m = 0;
      return 0;
    }
  l = *m;
  for (j = 1; j <= 15; j++)
    if (c[j])
      break;
  k = j;
  if ((unsigned int) l < j)
    l = j;
  for (i = 15; i; i--)
    if (c[i])
      break;
  g = i;
  if ((unsigned int) l > i)
    l = i;
  *m = l;
  for (y = 1 << j; j < i; j++, y <<= 1)
    if ((y -= c[j]) < 0)
      return -3;
  if ((y -= c[i]) < 0)
    return -3;
  c[i] += y;
  x[1] = j = 0;
  p = c + 1;
  xx = x + 2;
  while (--i)
    *xx++ = (j += *p++);
  p = b;
  i = 0;
  do
    if ((j = *p++) != 0)
      v[x[j]++] = i;
  while (++i < n);
  n = x[g];
  x[0] = i = 0;
  p = v;
  h = -1;
  w = -l;
  u[0] = (A *) 0;
  q = (A *) 0;
  z = 0;
  for (; k <= g; k++)
    {
      a = c[k];
      while (a--)
	{
	  while (k > w + l)
	    {
	      h++;
	      w += l;
	      z = g - w;
	      z = z > (unsigned int) l ? l : z;
	      if ((f = 1 << (j = k - w)) > a + 1)
		{
		  f -= a + 1;
		  xx = c + k;
		  if (j < z)
		    while (++j < z)
		      {
			if ((f <<= 1) <= *++xx)
			  break;
			f -= *xx;
		      }
		}
	      z = 1 << j;
	      if (*hn + z > 1440)
		return -3;
	      u[h] = q = hp + *hn;
	      *hn += z;
	      if (h)
		{
		  x[h] = i;
		  r.a0.a2.a4 = (unsigned char) l;
		  r.a0.a2.a3 = (unsigned char) j;
		  j = i >> (w - l);
		  r.a1 = (unsigned int) (q - u[h - 1] - j);
		  u[h - 1][j] = r;
		}
	      else
		*t = q;
	    }
	  r.a0.a2.a4 = (unsigned char) (k - w);
	  if (p >= v + n)
	    r.a0.a2.a3 = 128 + 64;
	  else if (*p < s)
	    {
	      r.a0.a2.a3 = (unsigned char) (*p < 256 ? 0 : 32 + 64);
	      r.a1 = *p++;
	    }
	  else
	    {
	      r.a0.a2.a3 = (unsigned char) (e[*p - s] + 16 + 64);
	      r.a1 = d[*p++ - s];
	    }
	  f = 1 << (k - w);
	  for (j = i >> w; j < z; j += f)
	    q[j] = r;
	  for (j = 1 << (k - 1); i & j; j >>= 1)
	    i ^= j;
	  i ^= j;
	  ee = (1 << w) - 1;
	  while ((i & ee) != x[h])
	    {
	      h--;
	      w -= l;
	      ee = (1 << w) - 1;
	    }
	}
    }
  return y != 0 && g != 1 ? (-5) : 0;
}

int a[19] = { 3, 4, 0, 2, 2, [17] = 3, 3 };
int d[19];
A h[1440];

int
main (void)
{
  int b = 0, c = 0;
  A *e = 0;
  foo (a, 19, 19, 0, 0, &e, &b, h, &c, d);
  exit (0);
}
