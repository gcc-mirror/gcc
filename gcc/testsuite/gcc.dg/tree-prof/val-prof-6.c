/* { dg-options "-O2 -fdump-tree-optimized" } */
char a[1000];
char b[1000];
int size=1000;
__attribute__ ((noinline))
t(int size)
{
  __builtin_memcpy(a,b,size);
}
int
main()
{
  int i;
  for (i=0; i < size; i++)
    t(i);
  return 0;
}
/* { dg-final-use { scan-tree-dump "Average value sum:499500" "optimized"} } */
/* { dg-final-use { scan-tree-dump "IOR value" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
