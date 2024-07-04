/* PR tree-optimization/113753 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 129
unsigned _BitInt(128)
foo (unsigned u, unsigned _BitInt(128) a, unsigned _BitInt(128) b)
{
  unsigned _BitInt(129) m = a % b;
  return u * m / u;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 129
  if (foo (0xfa637c33, 0x37af7fe8b0000000000000000wb,
	   0xfffffffff0000000000000000wb)
      != 0x16f7e93f6d726b38b38d0b753wb)
    __builtin_abort ();
#endif
}
