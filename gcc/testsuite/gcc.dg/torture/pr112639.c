/* PR middle-end/112639 */
/* { dg-do run } */

unsigned long long b = 0;

int
foo (void)
{
  return __builtin_clzg (b++, __SIZEOF_LONG_LONG__ * __CHAR_BIT__);
}

int
bar (void)
{
  return __builtin_ctzg (b++, __SIZEOF_LONG_LONG__ * __CHAR_BIT__);
}

int
main ()
{
  if (foo () != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ || b != 1)
    __builtin_abort ();
  if (foo () != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1 || b != 2)
    __builtin_abort ();
  if (foo () != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 2 || b != 3)
    __builtin_abort ();
  b = 0;
  if (bar () != __SIZEOF_LONG_LONG__ * __CHAR_BIT__ || b != 1)
    __builtin_abort ();
  if (bar () != 0 || b != 2)
    __builtin_abort ();
  if (bar () != 1 || b != 3)
    __builtin_abort ();
}
