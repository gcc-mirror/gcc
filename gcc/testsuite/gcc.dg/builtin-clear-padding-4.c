/* PR middle-end/128302 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 65535
struct C { _BitInt(65535) a : 65531; unsigned _BitInt(65535) b : 65531; } c1, c2, c3, c4;
#else
struct C { int a : 5; unsigned b : 5; } c1, c2;
#endif

__attribute__((noipa)) void
foo (struct C *c)
{
  c->a = 0;
  c->b = 0;
}

__attribute__((noipa)) void
bar (struct C *c)
{
  c->a = -1;
  c->b = -1;
}

int
main ()
{
  __builtin_memset (&c2, -1, sizeof (c2));
  __builtin_memset (&c4, -1, sizeof (c4));
  foo (&c1);
  foo (&c2);
  bar (&c3);
  bar (&c4);
  __builtin_clear_padding (&c2);
  __builtin_clear_padding (&c4);
  if (__builtin_memcmp (&c1, &c2, sizeof (c1))
      || __builtin_memcmp (&c3, &c4, sizeof (c3)))
    __builtin_abort ();
  return 0;
}
