/* { dg-options "(-mips16) -mabi=64 -EB" } */

extern long double g[16];
extern unsigned char gstuff[0x10000];

NOMIPS16 long double
foo (long double i1, long double i2, long double i3, long double i4,
     long double *x, unsigned char *lstuff)
{
  g[0] = i1;
  g[1] = i2;
  g[2] = i3;
  g[3] = i4;
  x[0] = x[4];
  x[1] = 0;
  x[2] = 1.0;
  x[3] = g[4];
  x[4] = *(long double *) (lstuff + 0x7fff);
  return *(long double *) (gstuff + 0x7fff);
}

MIPS16 long double
bar (long double i1, long double i2, long double i3, long double i4,
     long double *x, unsigned char *lstuff)
{
  g[0] = i1;
  g[1] = i2;
  g[2] = i3;
  g[3] = i4;
  x[0] = x[4];
  x[1] = 0;
  x[2] = 1.0;
  x[3] = g[4];
  x[4] = *(long double *) (lstuff + 0x7fff);
  return *(long double *) (gstuff + 0x7fff);
}
