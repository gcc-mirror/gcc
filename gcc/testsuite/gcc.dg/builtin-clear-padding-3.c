/* PR middle-end/128302 */
/* { dg-do run } */
/* { dg-options "-O2" } */

union A { int a : 15; unsigned b : 15; } a1, a2;
#if __BITINT_MAXWIDTH__ >= 951
union B { _BitInt(951) a : 739; unsigned _BitInt(951) b : 739; } b1, b2;
#else
union B { int a : 5; unsigned b : 5; } b1, b2;
#endif
#if __BITINT_MAXWIDTH__ >= 65535
union C { _BitInt(65535) a : 65531; unsigned _BitInt(65535) b : 65531; } c1, c2;
#else
union C { int a : 5; unsigned b : 5; } c1, c2;
#endif

__attribute__((noipa)) void
foo (union A *a, union B *b, union C *c)
{
  a->a = -1361;
#if __BITINT_MAXWIDTH__ >= 951
  b->a = 878212441949088910277769330824589426446069476594189768023517947696484604789122065870316179286548943333278002443986694487060345108557457062615946090733223961247635344029067055511695459419265707468087255861705908585308874670wb;
#else
  b->a = 9;
#endif
  c->a = 0;
}

int
main ()
{
  __builtin_memset (&a2, -1, sizeof (a2));
  __builtin_memset (&b2, -1, sizeof (b2));
  __builtin_memset (&c2, -1, sizeof (c2));
  foo (&a1, &b1, &c1);
  foo (&a2, &b2, &c2);
  __builtin_clear_padding (&a2);
  __builtin_clear_padding (&b2);
  __builtin_clear_padding (&c2);
  if (__builtin_memcmp (&a1, &a2, sizeof (a1))
      || __builtin_memcmp (&b1, &b2, sizeof (b1))
      || __builtin_memcmp (&c1, &c2, sizeof (c1)))
    __builtin_abort ();
  return 0;
}
