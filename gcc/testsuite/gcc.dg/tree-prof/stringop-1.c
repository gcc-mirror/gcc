/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile-optimized" } */
int a[1000];
int b[1000];
int size=1;
int max=10000;
int
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
/* autofdo doesn't support value profiling for now: */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Transformation done: single value 4 stringop" "profile"} } */
/* Really this ought to simplify into assignment, but we are not there yet.  */
/* a[0] = b[0] is what we fold the resulting memcpy into.  */
/* { dg-final-use-not-autofdo { scan-tree-dump " = MEM.*&b" "optimized"} } */
/* { dg-final-use-not-autofdo { scan-tree-dump "MEM.*&a\\\] = " "optimized"} } */
