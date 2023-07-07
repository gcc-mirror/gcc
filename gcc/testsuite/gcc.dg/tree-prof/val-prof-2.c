/* { dg-options "-O2 -fdump-tree-optimized-details-blocks -fdump-ipa-profile-optimized" } */
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
/* { dg-final-use-not-autofdo { scan-ipa-dump "Transformation done: div/mod by constant 256" "profile" } } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
