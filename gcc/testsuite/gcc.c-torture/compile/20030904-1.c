struct A
{
  long a1;
  double *a2;
};

struct B
{
  void *b1;
  double b2, b3;
  struct
  {
    int d1;
    double d2;
  } b4;
};

struct C
{
  struct A *c1;
  void *c2;
};

long fn1 (struct A *, double);
void fn2 (void *, const char *);
double fn3 (double);
double fn4 (double);
int fn5 (void *, double, double);

int
foo (struct B *x)
{
  struct C *e = x->b1;
  struct A *f = e->c1;
  long g, h, i;
  double *j, k;
  g = fn1 (f, 0.5 * (x->b2 + x->b3)), h = g + 1, i = f->a1;
  j = f->a2, k = x->b4.d2;
  fn2 (x, "something");
  if (g <= 0)
    {
      double l = j[2] - j[1];
      if (l > 0.0 && l <= 0.02)
        k = (x->b4.d1 == 1
             ? ((1.0 / l) < 25 ? 25 : (1.0 / l))
             : fn3 ((1.0 / l) < 25 ? 25 : (1.0 / l)));
    }
  else
    {
      double m = j[h] - j[g], n = 0.0, l = 0.0;
      if (g > 1)
        n = j[g] - j[g - 1];
      if (h < i)
        l = j[h + 1] - j[h];
      if (n > 0.02)
        n = 0;
      if (m > 0.02)
        m = 0;
      if (l > 0.02)
        l = 0;
      if (m < n)
        {
          double o = m;
          m = n;
          n = o;
        }
      if (l < n)
        {
          double o = l;
          l = n;
          n = o;
        }
      if (l < m)
        {
          double o = l;
          l = m;
          m = o;
        }
      if (n != 0.0)
        k = (x->b4.d1 == 1
             ? ((1 / m) < 25 ? 25 : (1 / m))
             : fn3 ((1 / m) < 25 ? 25 : (1 / m)));
      else if (m != 0.0)
        k = (x->b4.d1 == 1
             ? ((2 / (m + l)) < 25 ? 25 : (2 / (m + l)))
             : fn3 ((2 / (m + l)) < 25 ? 25 : (2 / (m + l))));
      else if (l != 0.0)
        k = (x->b4.d1 == 1
             ? ((1 / l) < 25 ? 25 : (1 / l))
             : fn3 ((1 / l) < 25 ? 25 : (1 / l)));
    }
  fn5 (e->c2, 0.5 * (x->b2 + x->b3), (x->b4.d1 == 1 ? k : fn4 (k)));
  return 1;
}
