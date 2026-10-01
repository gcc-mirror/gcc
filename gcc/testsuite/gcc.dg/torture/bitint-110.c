/* PR middle-end/127669 */
/* { dg-do run { target bitint } } */

#if __BITINT_MAXWIDTH__ >= 129
typedef unsigned _BitInt(129) T;

[[gnu::noipa]] static T
foo (T v)
{
  return v;
}

static T
bar ()
{
  return 1;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 129
  T x = __builtin_bitreverseg ((T) 1);
  T y = __builtin_bitreverseg (bar ());
  T z = __builtin_bitreverseg (foo (1));
  if (x != y || x != z)
    __builtin_abort ();
#endif
}
