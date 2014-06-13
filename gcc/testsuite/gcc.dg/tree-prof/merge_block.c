
/* { dg-options "-O2 -fno-ipa-pure-const -fdump-tree-optimized-blocks-details -fno-early-inlining" } */
int a[8];
int t()
{
	int i;
	for (i = 0; i < 3; i++)
		if (a[i])
			break;
	return i;
}
main ()
{
  int i;
  /* The loop will be optimized away after ipa-inline.  */
  for (i = 0; i < 1000; i++)
    t ();
  return 0;
}
/* { dg-final-use { scan-tree-dump-not "Invalid sum" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
