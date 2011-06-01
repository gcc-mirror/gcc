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
      __builtin_memcpy (a, b, size * sizeof (a[0]));
      asm("");
    }
   return 0;
}
/* { dg-final-use { scan-ipa-dump "Single value 4 stringop" "profile"} } */
/* Really this ought to simplify into assignment, but we are not there yet.  */
/* a[0] = b[0] is what we fold the resulting memcpy into.  */
/* { dg-final-use { scan-tree-dump " = MEM.*&b" "optimized"} } */
/* { dg-final-use { scan-tree-dump "MEM.*&a\\\] = " "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
/* { dg-final-use { cleanup-ipa-dump "profile" } } */
