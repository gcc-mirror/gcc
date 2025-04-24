/* PR target/119873 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

const char *foo (void *, void *, void *, void *, unsigned long, unsigned long);

const char *
bar (void *a, void *b, void *c, void *d, unsigned long e, unsigned long f)
{
  [[gnu::musttail]] return foo (a, b, c, d, e, f);
}
