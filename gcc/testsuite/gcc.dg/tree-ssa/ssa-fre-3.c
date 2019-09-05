/* This test requires:

      TRULY_NOOP_TRUNCATION (sizeof (int) * CHAR_BIT,
			     sizeof (long long) * CHAR_BIT)

   When the condition is true, we distribute "(int) (a + b)" as
   "(int) a + (int) b", otherwise we keep the original.  */
/* { dg-do compile { target { ! mips64 } } } */
/* { dg-options "-O -fno-tree-forwprop -fno-tree-ccp -fwrapv -fdump-tree-fre1-details" } */

/* From PR14844.  */

int
foo (int a, int b)
{
  long long aa = a;
  long long bb = b;
  return aa + bb;
}

/* { dg-final { scan-tree-dump "Replaced \\\(int\\\) aa_.*with a_" "fre1" { xfail { riscv*-*-* && lp64 } } } } */
