/* PR tree-optimization/116093 */
/* { dg-do run { target bitint } } */
/* { dg-options "-Og -ftree-vrp -fno-tree-dce" } */

#if __BITINT_MAXWIDTH__ >= 129
char
foo (int a, _BitInt (129) b, char c)
{
  return c << (5 / b % (0xdb75dbf5 | a));
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 129
  if (foo (0, 6, 1) != 1)
    __builtin_abort ();
#endif
}
