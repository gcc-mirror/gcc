/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-local-pure-const1 -fdump-tree-optimized" } */
static __attribute__ ((noinline, noclone))
int i_am_pure(char *c, int n)
{
  char *d=__builtin_alloca (n);
  int i;
  int sum = 0;
  for (i=0;i<n;i++)
    d[i] = c[i];
  for (i=0;i<n;i++)
    d[i] *= c[n-i];
  for (i=0;i<n;i++)
    sum+=d[i];
  if (sum)
    __builtin_unreachable ();
  return sum;
}
char array[11];
int
main(void)
{
  i_am_pure (array,5);
  i_am_pure (array,11);
  return 0;
}
/* { dg-final { scan-tree-dump "found to be pure: i_am_pure" "local-pure-const1"} } */
/* { dg-final { scan-tree-dump-not "i_am_pure" "optimized"} } */
