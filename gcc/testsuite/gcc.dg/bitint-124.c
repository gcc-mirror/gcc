/* PR tree-optimization/121131 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 156
struct A { _BitInt(156) b : 135; };

static inline _BitInt(156)
foo (struct A *x)
{
  return x[1].b;
}

__attribute__((noipa)) _BitInt(156)
bar (void)
{
  struct A a[] = { 1, 1, -13055525270329736316393717310914023773847wb,
		   1, 1, 1, 1, 1, 1, 1, 1, 1 };
  return foo (&a[1]);
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 156
  if (bar () != -13055525270329736316393717310914023773847wb)
    __builtin_abort ();
#endif
}
