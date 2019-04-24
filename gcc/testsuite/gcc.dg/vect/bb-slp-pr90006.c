/* { dg-do compile } */
/* { dg-additional-options "-fno-math-errno" } */
/* { dg-additional-options "-march=x86-64" { target x86_64-*-* i?86-*-* } } */

long int lrint(double x);

int a, b;
union c {
    int d;
};

int e()
{
  int f, g, h;
  long i, j, k;
  double l, m = b = lrint(0.3127);
  a = b >> 16 >> 8 & 255;
  ((union c *)e)->d = a;
  k = m;
  h = k >> 16 >> 8 & 255;
  ((union c *)(e + 4))->d = h;
  j = lrint(l);
  g = j >> 16 >> 8 & 255;
  ((union c *)(e + 8))->d = g;
  i = lrint(0.292);
  f = i >> 16 >> 8 & 255;
  ((union c *)(e + 12))->d = f;
  return 0;
}

/* { dg-final { scan-tree-dump "basic block vectorized" "slp2" { target { { x86_64-*-* i?86-*-* } && ilp32 } } } } */
