/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-tree_profile" } */
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
/* { dg-final-use { scan-tree-dump "Single value 4 stringop" "tree_profile"} } */
/* Really this ought to simplify into assignment, but we are not there yet.  */
/* { dg-final-use { scan-tree-dump "memset.*4\\)" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
/* { dg-final-use { cleanup-tree-dump "tree_profile" } } */
