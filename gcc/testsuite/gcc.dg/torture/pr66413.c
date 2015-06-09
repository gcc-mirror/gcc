/* { dg-do compile } */
/* { dg-additional-options "-g" } */

int a, b, c, d, i, j, q, *e, *h, *k, *r, **p = &e;
const int *f, **n = &f;
static int g;

void
fn1 (int p1)
{
  c = p1;
}

static int *
fn2 (int *p1, const int *p2)
{
  if (g)
    n = &p2;
  *n = p2;
  int o[245];
  fn1 (o != p2);
  return p1;
}

static int *
fn3 ()
{
  int s[54], *t = &s[0], u = 0, v = 1;
  h = &v;
  q = 1;
  for (; q; q++)
    {
      int *w[] = { &u };
      for (; v;)
	return *p;
    }
  *r = *t + b >= 0;
  return *p;
}

static int
fn4 (int *p1)
{
  int *l[2], **m[7];
  for (; i < 1; i++)
    for (; j < 1; j++)
      m[i * 70] = &l[0];
  k = fn3 ();
  fn2 (0, p1);
  if ((m[0] == 0) & a)
    for (;;)
      ;
  return 0;
}

int
main ()
{
  fn4 (&d);
  return 0;
}
