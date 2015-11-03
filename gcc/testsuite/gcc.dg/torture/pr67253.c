/* { dg-do run } */

int *a, b, c, **d = &a, e, f, **h, i, j, k, l, m, *n, o, **q, r, s;

void fn1 (int p) { }

void
fn3 ()
{
  for (; j; j++)
    for (; k; k++)
      l++;
  f++;
}

static int
fn4 (char p1, int *p2)
{
  for (; m < 1;)
    {
      fn1 (q == &p2);
      for (; o; o++)
	;
      n = p2;
      return 0;
    }
  for (;;)
    {
      for (; s; s++)
	b = r;
      *d = 0;
    }
}

static int *fn2 (char, int, int *);

static int
fn5 ()
{
  int *g = &c;
  fn3 ();
  fn2 (0, 0, g);
  return e;
}

static int *
fn2 (char p1, int p2, int *p3)
{
  fn4 (0, p3);
  fn1 (&p3 == h);
  for (; i;)
    fn5 ();
  fn4 (0, p3);
  return *d;
}

int
main ()
{
  fn5 ();
  return 0;
}
