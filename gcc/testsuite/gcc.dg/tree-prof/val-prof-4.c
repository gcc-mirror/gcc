/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile" } */
unsigned int a[1000];
unsigned int b = 999;
unsigned int c = 1002;
unsigned int d = 1003;
int
main ()
{
  int i;
  unsigned int n;
  for (i = 0; i < 1000; i++)
    {
	    a[i]=1000+i;
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
/* autofdo does not do value profiling so far */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Transformation done: mod subtract" "profile" } } */
/* This is part of code checking that n is greater than the divisor so we are sure that it
   didn't get optimized out.  */
/* { dg-final-use-not-autofdo { scan-tree-dump "if \\(n_\[0-9\]* \\>" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
