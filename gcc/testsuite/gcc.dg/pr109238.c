/* PR tree-optimization/109238 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

void foo (void *) __attribute__((noreturn));
void bar (void *);

void
baz (void *p)
{
  void *c = __builtin_realloc (p, 16);
  if (c)
    foo (c);
  for (;;)
    bar (__builtin_realloc (p, 8));	/* { dg-bogus "pointer 'p' may be used after '__builtin_realloc'" } */
}
