/* PR libgcc/114397 */
/* { dg-do run { target bitint } } */
/* { dg-options "-std=c23" } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __BITINT_MAXWIDTH__ >= 129
int
foo (unsigned _BitInt (128) a, _BitInt (129) b)
{
  return a / b;
}
#endif

#if __BITINT_MAXWIDTH__ >= 192
int
bar (unsigned _BitInt (128) a, _BitInt (192) b)
{
  return a / b;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 129
  if (foo (336225022742818342628768636932743029911uwb,
	   -336225022742818342628768636932743029911wb) != -1
      || foo (336225022742818342628768636932743029912uwb,
	      -336225022742818342628768636932743029911wb) != -1
      || foo (336225022742818342628768636932743029911uwb,
	      -336225022742818342628768636932743029912wb) != 0)
    __builtin_abort ();
#endif
#if __BITINT_MAXWIDTH__ >= 192
  if (bar (336225022742818342628768636932743029911uwb,
	   -336225022742818342628768636932743029911wb) != -1
      || bar (336225022742818342628768636932743029912uwb,
	      -336225022742818342628768636932743029911wb) != -1
      || bar (336225022742818342628768636932743029911uwb,
	      -336225022742818342628768636932743029912wb) != 0)
    __builtin_abort ();
#endif
}
