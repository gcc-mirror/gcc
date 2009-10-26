/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-pre" } */
int db[100];
int a_global_var, fact;
int main()
{
  int i,j=0;
  do
    {
      for (i=0; i<100; ++i)
        db[i] = i;
      fact = a_global_var * i;
    }
  while (j++ < 100);
}
/* We want to have exactly one load (not two) from a_global_var,
   and we want that load to be into a PRE temporary.  */
/* { dg-final { scan-tree-dump-times "= a_global_var;" 1 "pre" } } */
/* { dg-final { scan-tree-dump "pretmp\[^\\n\]* = a_global_var;" "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
