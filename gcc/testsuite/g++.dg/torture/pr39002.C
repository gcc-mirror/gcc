// PR target/39002
// { dg-do run }

struct S
{
  double x;
  double y;
  double z;
};

double foo (S *, S *);
void bar (S *, S *, S *, double &, double &, double &);

double
foo (S *a1, S *a2)
{
  return __builtin_sqrt ((a1->x - a2->x) * (a1->x - a2->x)
			 + (a1->y - a2->y) * (a1->y - a2->y)
			 + (a1->z - a2->z) * (a1->z - a2->z));
}

void
bar (S *p, S *q, S *r, double &x, double &y, double &z)
{
  if (foo (p, q) == 0.0)
    {
      x = r->x;
      y = r->y;
      z = r->z;
      return;
    }
  if (foo (p, r) == 0.0)
    {
      x = r->x;
      y = r->y;
      z = r->z;
      return;
    }
  if (foo (q, r) == 0.0)
    {
      x = r->x;
      y = r->y;
      z = r->z;
      return;
    }

  double a1, b1, c1, d1, e1;
  double dx, dy, dz, dw, dv;

  a1 = q->x - p->x;
  b1 = q->y - p->y;
  c1 = q->z - p->z;
  e1 = __builtin_sqrt (a1 * a1 + b1 * b1 + c1 * c1);
  a1 = a1 / e1;
  b1 = b1 / e1;
  c1 = c1 / e1;
  dx = p->x - r->x;
  dy = p->y - r->y;
  dz = p->z - r->z;
  dw = dx * dx + dy * dy + dz * dz;
  dv = 2.0 * dx * a1 + 2.0 * dy * b1 + 2.0 * dz * c1;
  d1 = -dv / 2.0;
  x = p->x + (a1 * d1);
  y = p->y + (b1 * d1);
  z = p->z + (c1 * d1);
  return;
}

int
main (void)
{
  S a, b, c, d, *p, *q, *r;

  p = &a;
  q = &b;
  r = &c;
  a.x = 0.0;
  a.y = 0.0;
  a.z = 0.0;
  b.x = 1.0;
  b.y = 0.0;
  b.z = 0.0;
  c.x = 0.0;
  c.y = 1.0;
  c.z = 0.0;
  bar (p, q, r, d.x, d.y, d.z);
  return 0;
}
