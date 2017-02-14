/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile" } */
unsigned int a[1000];
unsigned int b = 256;
unsigned int c = 1024;
unsigned int d = 17;
int
main ()
{
  int i;
  unsigned int n;
  for (i = 0; i < 1000; i++)
    {
	    a[i]=100*i;
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
/* { dg-final-use-not-autofdo { scan-ipa-dump "Mod power of 2 transformation on insn" "profile" } } */
/* This is part of code checking that n is power of 2, so we are sure that the transformation
   didn't get optimized out.  */
/* { dg-final-use-not-autofdo { scan-tree-dump "n_\[0-9\]* \\+ (4294967295|0x0*ffffffff)" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
