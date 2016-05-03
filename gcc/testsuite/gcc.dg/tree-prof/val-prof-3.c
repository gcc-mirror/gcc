/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile" } */
unsigned int a[1000];
unsigned int b = 257;
unsigned int c = 1023;
unsigned int d = 19;
int
main ()
{
  int i;
  unsigned int n;
  for (i = 0; i < 1000; i++)
    {
	    a[i]=18;
    }
  for (i = 0; i < 1000; i++)
    {
      if (i % 2)
	n = b;
      else if (i % 3)
	n = c;
      else
	n = d;
      a[i] %= n;
    }
  return 0;
}
/* { dg-final-use { scan-ipa-dump "Mod subtract transformation on insn" "profile" } } */
/* This is part of code checking that n is greater than the divisor so we are sure that it
   didn't get optimized out.  */
/* { dg-final-use { scan-tree-dump "if \\(_\[0-9\]* \\< n_\[0-9\]*" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
