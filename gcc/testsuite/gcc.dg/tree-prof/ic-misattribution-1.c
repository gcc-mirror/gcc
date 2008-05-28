/* { dg-options "-O2 -fdump-tree-tree_profile" } */
/* { dg-additional-sources "ic-misattribution-1a.c" } */

extern void other_caller (void);

void
callee (void)
{
  return;
}

void
caller(void (*func) (void))
{
  func ();
}

/* { dg-final-use { scan-tree-dump "hist->count 1 hist->all 1" "tree_profile" } } */
/* { dg-final-use { cleanup-tree-dump "tree_profile" } } */
