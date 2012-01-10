/* PR debug/51695 */
/* { dg-do compile { target { int32plus } } } */
/* { dg-options "-O2 -g" } */

typedef struct
{
  struct { unsigned int t1, t2, t3, t4, t5, t6; } t;
  int p;
  struct { double X, Y, Z; } r;
} T;
typedef struct { T *h; } S;

static unsigned int v = 0x12345678;

int
foo (void)
{
  v = (v & 0x80000000) ? ((v << 1) ^ 0xa398655d) : (v << 1);
  return 0;
}

double
bar (void)
{
  unsigned int o;
  v = (v & 0x80000000) ? ((v << 1) ^ 0xa398655d) : (v << 1);
  o = v & 0xffff;
  return (double) o / 32768.0;
}

int
baz (void)
{
  foo ();
  return 0;
}

void
test (S *x)
{
  T *t = x->h;
  t->t.t1 = foo ();
  t->t.t2 = foo ();
  t->t.t3 = foo ();
  t->t.t4 = foo ();
  t->t.t5 = foo ();
  t->t.t6 = foo ();
  t->p = baz ();
  t->r.X = bar ();
  t->r.Y = bar ();
  t->r.Z = bar ();
}
