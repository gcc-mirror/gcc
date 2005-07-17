/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-eustores-all" } */
static int a;
int foo()
{
  int alocal;
  int b;
  alocal = a;
  b = alocal;
  a = b;
}
/* We should eliminate the store back to a.  */
/* { dg-final { scan-tree-dump-times "Eliminating useless store" 1 "eustores"} } */
/* { dg-final { cleanup-tree-dump "eustores" } } */
