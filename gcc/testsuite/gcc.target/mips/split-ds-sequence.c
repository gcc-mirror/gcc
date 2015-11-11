/* { dg-options "isa_rev>=6" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-mcompact-branches=never" } { "" } } */
/* { dg-final { scan-assembler-not "nop" } } */

int
testg2 (int a, int c)
{

  int j = 0;
  do
    {
      j += a;
    }
  while (j < 56);

  j += c;
  return j;

}
