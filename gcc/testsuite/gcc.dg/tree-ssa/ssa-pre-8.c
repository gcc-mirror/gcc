/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-pre-stats" } */
struct s {
      int *n;
};

int
foo (unsigned int i, struct s *array)
{
  int *p = array[i].n;
  if (p)
    {
      int *q = array[i].n;
      if (p != q)
	return 1;
    }
  return 0;
}
/* We should eliminate two address calculations, one cast, and one load.  */
/* { dg-final { scan-tree-dump-times "Eliminated: 4" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
