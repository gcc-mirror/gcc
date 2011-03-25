/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-fre1-stats" } */
int main(int argc, char **argv)
{
  int *p;
  int result;
  *p = 2;
  if (argc)
    *p = 2;
  result = *p;
  return result;
}
/* We should eliminate result = *p by saying it has the value 2.  */
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "fre1"} } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
