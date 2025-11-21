/* PR target/118358.  */
/* { dg-do compile } */
/* { dg-options "-O2" } */

extern void exit (int);

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
  for (j = 1; j <= 15; j++)
    if (c[j])
      break;
  if ((unsigned int) l < j)
    l = j;
  for (i = 15; i; i--)
    if (c[i])
      break;
  g = i;
  for (y = 1 << j; j < i; j++, y <<= 1)
    if ((y -= c[j]) < 0)
      return -3;
  h = -1;
  z = 0;
  for (; k <= g; k++)
    {
          while (k > w + l)
            {
              h++;
              w += l;
              if ((f = 1 << (j = k - w)) > a + 1)
                ;
              z = 1 << j;
              if (*hn + z > 1440)
                return -3;
              u[h] = q = hp + *hn;
                  r.a0.a2.a4 = (unsigned char) l;
                  r.a1 = (unsigned int) (q - u[h - 1] - j);
            }
          for (j = i >> w; j < z; j += f)
            q[j] = r;
          for (j = 1 << (k - 1); i & j; j >>= 1)
            i ^= j;
          while ((i & ee) != x[h])
            {
              h--;
              w -= l;
            }
    }
  return y != 0 && g != 1 ? (-5) : 0;
}

unsigned int a[19] = { 3, 4, 0, 2, 2, [17] = 3, 3 };
unsigned int d[19];
A h[1440];

int
main (void)
{
  unsigned int b = 0, c = 0;
  A *e = 0;
  foo (a, 19, 19, 0, 0, &e, &b, h, &c, d);
  exit (0);
}
