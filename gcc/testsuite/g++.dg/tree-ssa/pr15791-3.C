/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

int f(int i, unsigned j)
{
      int b[2];
      if (&b[i] == &b[j])
	      return 1;
      return 0;
}

/* { dg-final { scan-tree-dump-times "i == j" 0 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
