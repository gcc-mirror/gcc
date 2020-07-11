/* PR tree-optimization/94956 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "\tcmovne\t" } } */
/* { dg-final { scan-assembler-not "\tsete\t" } } */

int
foo (unsigned x)
{
  if (x == 0) __builtin_unreachable ();
  return __builtin_ffs (x) - 1;
}

int
bar (unsigned long x)
{
  if (x == 0) __builtin_unreachable ();
  return __builtin_ffsl (x) - 1;
}

#ifdef __x86_64__
int
baz (unsigned long long x)
{
  if (x == 0) __builtin_unreachable ();
  return __builtin_ffsll (x) - 1;
}
#endif
