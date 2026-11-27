/* PR middle-end/128930 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O1" } */

#if __BITINT_MAXWIDTH__ >= 256
_BitInt(256)
foo (_BitInt(256) a)
{
  return -a;
}

[[gnu::noipa]] int
bar (int t, _BitInt(192) y)
{
  _BitInt(129) l = foo (t);
  return l > y;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 256
  if (bar (1, 0))
    __builtin_abort ();
#endif
}
