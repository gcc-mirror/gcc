/* { dg-do compile  { target { ! { ia32 } } } } */
/* { dg-options "-O2" } */

volatile int a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p;

volatile long long y;

void
test ()
{
  int a_ = a;
  int b_ = b;
  int c_ = c;
  int d_ = d;
  int e_ = e;
  int f_ = f;
  int g_ = g;
  int h_ = h;
  int i_ = i;
  int j_ = j;
  int k_ = k;
  int l_ = l;
  int m_ = m;
  int n_ = n;
  int o_ = o;
  int p_ = p;

  int z;

  for (z = 0; z < 1000; z++)
    {
      __atomic_store_n (&y, 0x100000002ll, __ATOMIC_SEQ_CST);
      __atomic_store_n (&y, 0x300000004ll, __ATOMIC_SEQ_CST);
    }

  a = a_;
  b = b_;
  c = c_;
  d = d_;
  e = e_;
  f = f_;
  g = g_;
  h = h_;
  i = i_;
  j = j_;
  k = k_;
  l = l_;
  m = m_;
  n = n_;
  o = o_;
  p = p_;
}

/* { dg-final { scan-assembler-times "movabs" 2 } } */
