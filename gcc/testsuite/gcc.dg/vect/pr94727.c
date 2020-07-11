/* { dg-additional-options "-O3" } */

unsigned char a[16][32];
long b[16][32];
unsigned long c;
_Bool d;

void __attribute__((noipa))
foo (void)
{
  for (int j = 0; j < 8; j++)
    for (int i = 0; i < 17; ++i)
      b[j][i] = (a[j][i] < c) > d;
}

int
main (void)
{
  c = 1;
  foo ();
  if (!b[0][0])
    __builtin_abort ();
  return 0;
}
