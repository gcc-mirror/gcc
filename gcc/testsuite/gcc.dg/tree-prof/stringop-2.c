/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile" } */
int a[1000];
int b[1000];
int size=1;
int max=10000;
main()
{
  int i;
  for (i=0;i<max; i++)
    {
      __builtin_memset (a, 10, size * sizeof (a[0]));
      asm("");
    }
   return 0;
}
/* { dg-final-use { scan-ipa-dump "Single value 4 stringop" "profile"} } */
/* The versioned memset of size 4 should be optimized to an assignment.  */
/* { dg-final-use { scan-tree-dump "a\\\[0\\\] = 168430090" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
/* { dg-final-use { cleanup-ipa-dump "profile" } } */
