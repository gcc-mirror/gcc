/* PR tree-optimization/119707 */
/* { dg-do run { target bitint } } */

#if __BITINT_MAXWIDTH__ >= 256
__attribute__((noipa)) unsigned _BitInt(256)
foo (unsigned _BitInt(256) x, _BitInt(129) y)
{
  return x + (unsigned _BitInt(255)) y;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 256
  if (foo (0, -1) != 0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffuwb)
    __builtin_abort ();
#endif
}
