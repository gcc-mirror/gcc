/* { dg-do run } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

extern void abort (void);
extern void exit (int);

unsigned char a[256], b[256], c[256], d[256];

void foo(unsigned char *x, int y, unsigned char *z)
{
}

void bar(int x, ...)
{
}

void baz(int y)
{
  if (y != 0x10)
    abort();
}

void test(int x, unsigned char *y)
{
  unsigned char g,h,j, k[5],l[5], m[30];
  int i;

  bar(x, y[0], y[1], y[2], y[3], y[4], y[5], y[6], y[7], y[8], y[9]);
  for (i = 5; --i >= 0; )
    k[i] = y[5 + i] ^ a[i] ^ c[i];

  foo(&m[29], sizeof m, k);
  g = d[x] ^ c[x];
  bar(x, d[x], x, c[x]);
  baz(g);
  for (i = 5, h = 0; --i >= 0; h = y[i])
    {
      j = m[25 + i] ^ y[i];
      j = b[j] ^ g;
      k[i] = c[j] ^ h;
    }
  for (i = 5, h = 0; --i >= 0; h = k[i])
    {
      j = m[20 + i] ^ k[i];
      j = b[j] ^ g;
      l[i] = c[j] ^ h;
    }
  for (i = 5, h = 0; --i >= 0; h = l[i]) {
    j = m[15 + i] ^ l[i];
    j = b[j] ^ g;
    j = c[j] ^ h;
    k[i] = a[j] ^ c[j];
  }
}

int main()
{
  c[4] = 0xdc;
  d[4] = 0xcc;
  test(4, a);
  exit(0);
}
