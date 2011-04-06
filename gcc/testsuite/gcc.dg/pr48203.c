/* PR debug/48203 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

volatile int v;

void
foo (long a, long b, long c, long d, long e, long f, long g, long h,
     long i, long j, long k, long l, long m, long n, long o, long p)
{
  long a2 = a;
  long b2 = b;
  long c2 = c;
  long d2 = d;
  long e2 = e;
  long f2 = f;
  long g2 = g;
  long h2 = h;
  long i2 = i;
  long j2 = j;
  long k2 = k;
  long l2 = l;
  long m2 = m;
  long n2 = n;
  long o2 = o;
  long p2 = p;
  v++;
}

void
bar (int a, int b, int c, int d, int e, int f, int g, int h,
     int i, int j, int k, int l, int m, int n, int o, int p)
{
  int a2 = a;
  int b2 = b;
  int c2 = c;
  int d2 = d;
  int e2 = e;
  int f2 = f;
  int g2 = g;
  int h2 = h;
  int i2 = i;
  int j2 = j;
  int k2 = k;
  int l2 = l;
  int m2 = m;
  int n2 = n;
  int o2 = o;
  int p2 = p;
  v++;
}
