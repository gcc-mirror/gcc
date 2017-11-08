/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int x = 1;

int main ()
{
  int t = (1/(1>=x))>>1;
  if (t != 0) __builtin_abort();
  return 0;
}

/* { dg-final { scan-tree-dump "<bb 2> \\\[local count: \[0-9INV\]*\\\]:\[\n\r \]*return 0;" "optimized" } } */
