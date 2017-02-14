/* Check that multi-insn if-conversion is not done if the override
   parameter would not allow it.  Set the cost parameter very high
   to ensure that the limiting factor is actually the count parameter.  */

/* { dg-options "-fdump-rtl-ce1 -O2 --param max-rtl-if-conversion-insns=1 --param max-rtl-if-conversion-unpredictable-cost=200" } */

typedef int word __attribute__((mode(word)));

word
foo (word x, word y, word a)
{
  word i = x;
  word j = y;
  if (x > y)
    {
      i = a;
      j = i;
    }
  return i * j;
}
/* { dg-final { scan-rtl-dump "0 true changes made" "ce1" } } */
