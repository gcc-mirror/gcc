/* PR tree-optimization/80109 */
/* { dg-do compile } */
/* { dg-require-effective-target alloca } */
/* { dg-options "-O2 -Walloca-larger-than=126812070" } */

void
g (int *p)
{
  extern void f (void *);

  void *q = __builtin_alloca (p); /* { dg-warning "passing argument 1" } */
  /* { dg-warning "unbounded use of 'alloca'" "unbounded" { target *-*-* } .-1 } */
  f (q);
}
