/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dce2" } */

int t() __attribute__ ((const));
q()
{
  int i = t();
  if (!i)
    i = t();
}
/* There should be no IF conditionals.  */
/* { dg-final { scan-tree-dump-times "if " 0 "dce2"} } */
/* { dg-final { cleanup-tree-dump "dce2" } } */
