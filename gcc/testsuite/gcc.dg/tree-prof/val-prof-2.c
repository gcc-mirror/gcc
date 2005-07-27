/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-tree_profile" } */
unsigned int a[1000];
unsigned int b = 256;
unsigned int c = 1024;
unsigned int d = 17;
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
/* { dg-final-use { scan-tree-dump "Mod power of 2 transformation on insn" "tree_profile"} } */
/* This is part of code checking that n is power of 2, so we are sure that the transformation
   didn't get optimized out.  */
/* { dg-final-use { scan-tree-dump "n \\+ \\-1" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
/* { dg-final-use { cleanup-tree-dump "tree_profile" } } */
