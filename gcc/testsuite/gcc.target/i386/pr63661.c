/* PR target/63661 */
/* { dg-do run } */
/* { dg-require-effective-target fpic } */
/* { dg-options "-mtune=nehalem -fPIC -O2" } */

static void __attribute__((noinline,noclone,hot))
foo (double a, double q, double *ff, double *gx, int e, int ni)
{
  union
    {
      double n;
      unsigned long long o;
    } punner;
  double d;

  punner.n = q;
   __builtin_printf("B: 0x%016llx ---- %g\n", punner.o, q);

  d = q - 5;
  if(d < 0)
    d = -d;
  if (d > 0.1)
    __builtin_abort();
}

static int __attribute__((noinline,noclone,hot))
bar (int order, double q, double c[])
{
  int ni, nn, i, e;
  double g2, x2, de, s, ratio, ff;

  nn = 0;
  e = order & 1;
  s = 0;
  ratio = 0;
  x2 = 0;
  g2 = 0;

  if(q == 0.0)
    return 0;

  if (order < 5)
    {
      ratio = 1.0 / q;
      nn = order;
    }

  ni = -nn;

  while(1)
    {
      de = ratio - g2 - x2;

      foo (0, q, &ff, &g2, e, ni);

      if((int)de == 0)
        break;
    }

  s += 2 * nn * c[nn];

  for (i = 0; i < 1; i++)
    {
      c[0] = nn;
      for (; i < 10; i++)
        c[i] = 0.0;
      c[0] /= s;
    }

  return 0;
}

int
main ()
{
  double c[1000];

  bar (1, 5.0, c);
  return 0;
}
