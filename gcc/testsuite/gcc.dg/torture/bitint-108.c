/* PR c/127383 */
/* { dg-do run { target bitint } } */

#if __BITINT_MAXWIDTH__ >= 65
struct { unsigned _BitInt(65) f : 63; } s;
#endif

int
main ()
{
#if __SIZEOF_LONG_LONG__ * __CHAR_BIT__ == 64 && __BITINT_MAXWIDTH__ >= 65
  unsigned long long w = ~(unsigned _BitInt(63)) s.f;
  if (w != (-1ULL >> 1))
    __builtin_abort ();
#endif
}
