// PR tree-optimization/98568
// { dg-do compile }

char a[2];
char b[4];

void
foo (int x)
{
  a[1] = x >> 8;
  a[0] = x;
}

void
bar (long long x)
{
  b[3] = x >> 24;
  b[2] = x >> 16;
  b[1] = x >> 8;
  b[0] = x;
}

void
baz (int x)
{
  a[0] = x >> 8;
  a[1] = x;
}

void
qux (long long x)
{
  b[0] = x >> 24;
  b[1] = x >> 16;
  b[2] = x >> 8;
  b[3] = x;
}
