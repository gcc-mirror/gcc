static unsigned short
foo (unsigned char *x, int y)
{
  unsigned short r = 0;
  int i;
  for (i = 0; i < y; i++)
    r += x[i];
  return r;
}

int baz (int, unsigned short);

void
bar (unsigned char *x, unsigned char *y)
{
  int i;
  unsigned short key = foo (x, 0x10000);
  baz (0, 0);
  for (i = 0; i < 0x80000; i++)
    y[i] = x[baz (i, key)];
}
