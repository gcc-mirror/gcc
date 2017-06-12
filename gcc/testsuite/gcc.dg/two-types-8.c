/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

enum x { XYZ }
struct g { enum x a; }; /* { dg-error "expected ';', identifier or " } */

int f(struct g *x)
{
  return x->a == XYZ; /* { dg-bogus " has no member " } */
}
