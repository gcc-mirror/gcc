/* PR middle-end/119808 */
/* { dg-do run { target { bitint && fstack_protector } } } */
/* { dg-options "-O0 -ftree-coalesce-vars -fstack-protector-strong" } */

#if __BITINT_MAXWIDTH__ >= 129
_BitInt(129)
foo ()
{
  _BitInt(129) b = 0;
  _BitInt(8) a
    =__builtin_stdc_rotate_right (0x8c82111b5d2d37c57e9ada7213ed95a49uwb, b);
  return b;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 129
  _BitInt(129) x = foo ();
  if (x)
    __builtin_abort ();
#endif
}
