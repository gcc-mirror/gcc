/* PR target/119873 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

const char *foo (void *, void *, void *, void *, unsigned long, unsigned long);

const char *
bar (void *a, void *b, void *c, void *d, unsigned long e, unsigned long f)
{
  [[gnu::musttail]] return foo (a, b, c, d, e + 1, f);	/* { dg-error "cannot tail-call: target is not able to optimize the call into a sibling call" } */
}

const char *
baz (void *a, void *b, void *c, void *d, unsigned long e, unsigned long f)
{
  [[gnu::musttail]] return foo (a, b, c, d, f, e);	/* { dg-error "cannot tail-call: target is not able to optimize the call into a sibling call" } */
}
