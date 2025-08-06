/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 1024
constexpr _BitInt(1024) d = -541140097068598424394740839221562143161511518875518765552323978870598341733206554363735813878577506997168480201818027232521wb;
int c;

static inline void
foo (_BitInt(1024) b, _BitInt(1024) *r)
{
  if (c)
    b = 0;
  *r = b;
}

[[gnu::noipa]] void
bar (_BitInt(1024) y)
{
  if (y != d)
    __builtin_abort ();
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 1024
  _BitInt(1024) x;
  foo (d, &x);
  bar (x);
#endif
}
