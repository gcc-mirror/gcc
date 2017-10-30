/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

#define M (256)
#define N (512)

struct st
{
  int a[M][N];
  int c[M];
  int b[M][N];
};

void
foo (struct st *p)
{
  for (unsigned i = 0; i < M; ++i)
    {
      p->c[i] = 0;
      for (unsigned j = N; j > 0; --j)
	{
	  p->a[i][j - 1] = 0;
	  p->b[i][j - 1] = 0;
	}
    }
}

/* { dg-final { scan-tree-dump-times "Loop nest . distributed: split to 0 loops and 1 library" 1 "ldist" } } */
/* { dg-final { scan-tree-dump-times "__builtin_memset \\(.*, 0, 1049600\\);" 1 "ldist" } } */
