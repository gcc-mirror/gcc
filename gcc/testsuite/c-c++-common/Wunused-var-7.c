/* PR c++/44062 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

void
foo (void)
{
  int a, b, c, d, e, f, g;
  a = 1;
  b = 2;
  c = 3;
  d = 4;
  e = 5;
  f = 6;
  g = 7;
  a;			/* { dg-warning "no effect" } */
  b, 1;			/* { dg-warning "no effect" } */
  (void) c;
  (void) d, 1;		/* { dg-warning "no effect" } */
  e, f, 1;		/* { dg-warning "no effect" } */
  (void) g, f, 1;	/* { dg-warning "no effect" } */
}

void
bar (void)
{
  int a;
  int b;
  int c;		/* { dg-warning "set but not used" } */
  a = 1;
  b = 2;
  c = 3;
  c = ({ a++, b; });
}

void
baz (void)
{
  int a;
  int b;
  int c;
  int d;
  a = 1;
  b = 2;
  c = 3;
  d = 4;
  d, ( a++, b ), c;	/* { dg-warning "no effect" } */
}
