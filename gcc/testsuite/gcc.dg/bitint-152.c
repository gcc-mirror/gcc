/* PR middle-end/128994 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O2" } */

#if __BITINT_MAXWIDTH__ >= 129
typedef unsigned _BitInt(129) T;
#else
typedef unsigned _BitInt(64) T;
#endif

static T c[4];

[[gnu::noipa]] T
foo (T a)
{
  if ((236 < a ? 236 : a) < -a)
    c[a] = a;
  return a;
}

int
main ()
{
  if (foo ((T) -2) != (T) -2)
    __builtin_abort ();
}
