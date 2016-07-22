/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ldist" } */

void
foo (int *p)
{
  unsigned short i, j;

  for (i = 0; i < 100; i++)
    for (j = 1; j < 101; j++)
      {
	unsigned int index = 100 * i + j;
	p[index-1] = 0;
      }
}

/* Loop can be transformed into builtin memset since &p[...] is SCEV.  */
/* { dg-final { scan-tree-dump "builtin_memset" "ldist" } } */
