/* PR middle-end/116486 */
/* { dg-do run { target bitint } } */
/* { dg-options "-O2 -fno-tree-ccp" } */

unsigned u;

#if __BITINT_MAXWIDTH__ >= 129
#define N 0x100000000000000000000000000000000uwb
#else
#define N 0xffffffffffffffffuwb
#endif

int
foo (void)
{
  return __builtin_stdc_first_leading_one (u / N);
}

int
main ()
{
  int x = foo ();
  if (x)
    __builtin_abort ();
}
