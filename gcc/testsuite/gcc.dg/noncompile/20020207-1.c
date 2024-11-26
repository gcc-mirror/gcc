/* This testcase ICEd because statement expression type was set, but was not
   as used.  */

struct A {
  unsigned int a, b, c;
};

extern void abort (void);
extern void exit (int);

struct A bar (void)
{
  return (struct A) { 176, 52, 31 };
}

void baz (struct A *a)
{
  if (a->a != 176 || a->b != 52 || a->c != 31)
    abort ();
}

int main ()
{
  struct A d;

  d = ({ { bar (); } });		/* { dg-error "invalid use of void expression" } */
  d = ({ if (1) { bar (); } });		/* { dg-error "invalid use of void expression" } */
  d = ({ while (0) { bar (); } });	/* { dg-error "invalid use of void expression" } */
  d = ({ do { bar (); } while (0); });	/* { dg-error "invalid use of void expression" } */
  baz (&d);
  exit (0);
}
