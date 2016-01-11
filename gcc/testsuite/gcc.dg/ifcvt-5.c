/* Check that multi-insn if-conversion is not done if the override
   parameter would not allow it.  */

/* { dg-options "-fdump-rtl-ce1 -O2 --param max-rtl-if-conversion-insns=1" } */
int
foo (int x, int y, int a)
{
  int i = x;
  int j = y;
  if (x > y)
    {
      i = a;
      j = i;
    }
  return i * j;
}
/* { dg-final { scan-rtl-dump "0 true changes made" "ce1" } } */
