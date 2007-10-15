/* PR tree-optimization/33136 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

struct S
{
  struct S *a;
  int b;
  float f;
};

static struct S s;

static int *
__attribute__((noinline, const))
foo (void)
{
  return &s.b;
}

float
__attribute__((noinline))
bar (float *f)
{
  s.f = 1.0;
  *f = 4.0;
  return s.f;
}

int
__attribute__((noinline))
baz (int *x)
{
  s.b = 1;
  *x = 4;
  return s.b;
}

int
t (void)
{
  float f = 8.0;
  return bar (&f) + baz (foo ());
}

int
main (void)
{
  if (t () != 5)
    abort ();
  return 0;
}
