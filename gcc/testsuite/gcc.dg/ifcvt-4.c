/* { dg-options "-fdump-rtl-ce1 -O2 --param max-rtl-if-conversion-insns=3" } */
/* { dg-skip-if "Multiple set if-conversion not guaranteed on all subtargets" { "arm*-*-* powerpc64le*-*-* visium-*-*" } {"*"} { "" } }  */

int
foo (int x, int y, int a)
{
  int i = x;
  int j = y;
  /* Try to make taking the branch likely.  */
  __builtin_expect (x > y, 1);
  if (x > y)
    {
      i = a;
      j = i;
    }
  return i * j;
}
/* { dg-final { scan-rtl-dump "2 true changes made" "ce1" } } */
