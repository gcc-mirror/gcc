/* { dg-options "-O -fgraphite-identity" } */

void *foo(const void *a);

void bug48648()
{
  unsigned char a[2];
  long b;
  int i;

  for(i = 0; i < 2; i++) {
    if (b <= 0)
      a[i] = 0;
    else if (b >= 8)
      a[i] = 0;
    else
      a[i] = 0;
    b -= 8;
  }
  foo(&a);
}
