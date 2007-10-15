/* PR tree-optimization/33136 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

struct S
{
  void *a;
  int b[3];
  double *c;
};
static double d, e;

static struct S s;

static int *
__attribute__((noinline, const))
foo (void)
{
  return (int *) &s.b;
}

double *
__attribute__((noinline))
bar (double **f)
{
  s.c = &d;
  *f = &e;
  /* As nothing ever takes the address of any double * field in struct S,
     the write to *f can't alias with the s.c field.  */
  return s.c;
}

int
__attribute__((noinline))
baz (int *x)
{
  s.b[0] = 1;
  *x = 4;
  /* Function foo takes address of an int array field in struct S,
     so *x can alias with the s.b field (and it does in this testcase).  */
  return s.b[0];
}

int
__attribute__((noinline))
t (void)
{
  double *f = (double *) 0;
  return 10 * (bar (&f) != &d) + baz (foo ());
}

int
main (void)
{
  if (t () != 4)
    abort ();
  return 0;
}
