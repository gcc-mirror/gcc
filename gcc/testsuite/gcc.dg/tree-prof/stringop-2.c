/* { dg-options "-O2 -fdump-tree-optimized -fdump-ipa-profile" } */
int a[1000];
int b[1000];
int size=1;
int max=10000;
#ifdef __mips
/* We allow short memcpy()s for MIPS16.  */
int __attribute__((nomips16))
#else
int
#endif
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
/* autofdo doesn't support value profiling for now: */
/* { dg-final-use-not-autofdo { scan-ipa-dump "Single value 4 stringop" "profile"} } */
/* The versioned memset of size 4 should be optimized to an assignment.  */
/* { dg-final-use-not-autofdo { scan-tree-dump "MEM\\\[\\(void .\\)&a\\\] = 168430090" "optimized"} } */
