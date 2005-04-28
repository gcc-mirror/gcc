/* { dg-options "-O2 -fargument-noalias-global -fdump-tree-optimized" } */
int f;
void link_error ();

void g(int *i)
{
  *i = 0;
  f = 1;
  if (*i != 0)
    link_error ();
}


/* We should have removed the link_error on the tree level as we told GCC
   that *i cannot point to f via the option -fargument-noalias-global. */
/* { dg-final { scan-tree-dump-times "link_error" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
