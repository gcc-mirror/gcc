/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -mfpmath=sse -fno-pie" } */

extern double r[6];
extern double a[];

double
loop (int k, double x)
{
  int i;
  double t=0;
  for (i=0;i<6;i++)
    r[i] = x * a[i + k];
  for (i=0;i<6;i++)
    t+=r[5-i];
  return t;
}

/* Verify we end up with no loads from r.  */
/* { dg-final { scan-assembler-not "v\[ma\]\[^\t \]+\tr" } } */
