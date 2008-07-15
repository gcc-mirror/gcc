/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct g { long a; };
unsigned long f(struct g *a) { return *(unsigned long *)&a->a; }

struct A
{
  void *a;
};

int g(const struct A *x, long *y)
{
  typedef long __attribute__ ((may_alias)) long_a;
  *y = *(const long_a *) (&x->a);
  return 1;
}

void *a;

int
f0 (long *y)
{
  *y = *(const long *) &a; /* { dg-warning "will break" } */
  return 1;
}

int
f1 (long *y)
{
  typedef long __attribute__ ((may_alias)) long_a;
  *y = *(const long_a *) &a;
  return 1;
}

int
f2 (long *y)
{
  *y = *(const long *) &a; /* { dg-warning "will break" } */
  return 1;
}
