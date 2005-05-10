/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-store_ccp" } */
  

extern int board[];

void
findbestextension (int blah, int blah2)
{
  int defval;
  defval = def_val (board[blah2]);
  if (blah)
    defval = 0;
  foo (defval);
}

/* The argument to "foo" should be a variable, not a constant.  */
/* { dg-final { scan-tree-dump-times "foo .defval" 1 "store_ccp"} } */
/* { dg-final { cleanup-tree-dump "store_ccp" } } */
