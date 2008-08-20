/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dce2" } */

/* We should notice constantness of this function. */
static int __attribute__((noinline)) t(int a) 
{
	return a+1;
}
void q(void)
{
  int i = t(1);
  if (!i)
    i = t(1);
}
/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dce2"} } */
/* { dg-final { cleanup-tree-dump "dce2" } } */
