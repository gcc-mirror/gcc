/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dce3" } */

/* We should notice constantness of this function. */
int t(int a) 
{
	return a+1;
}
q()
{
  int i = t(1);
  if (!i)
    i = t(1);
}
/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dce3"} } */
/* { dg-final { cleanup-tree-dump "dce3" } } */
