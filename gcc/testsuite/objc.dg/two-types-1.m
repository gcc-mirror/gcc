/* { dg-do compile } */
/* { dg-options "-std=gnu89" } // suppress default -pedantic-errors */

@interface foo
struct f {}
struct g { int a; }; /* { dg-error "expected ';', identifier or " } */

- (struct f *) a;
- (struct g *) b;
@end

int f(struct g *x)
{
  return x->a; /* { dg-bogus " has no member " } */
}
