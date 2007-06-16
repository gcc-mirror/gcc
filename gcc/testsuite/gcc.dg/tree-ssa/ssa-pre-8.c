/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-fre-stats" } */
struct s {
      int *n;
};

int
foo (__SIZE_TYPE__ i, struct s *array)
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
/* We should eliminate two address calculations, and one load.  */
/* We used to eliminate a cast but that was before POINTER_PLUS_EXPR
   was added.  */
/* { dg-final { scan-tree-dump-times "Eliminated: 3" 1 "fre"} } */
/* { dg-final { cleanup-tree-dump "fre" } } */
