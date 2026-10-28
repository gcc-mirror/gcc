/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-vect-details" } */

void
addxor (const unsigned int *__restrict a, unsigned int *__restrict out,
	unsigned int m, unsigned int n)
{
  if (n == 0)
    return;
  for (unsigned int r = 0; r < m; ++r)
    {
      unsigned int h = r + 1;
      for (unsigned int i = 0; i < n; ++i)
	h = (h + a[i]) ^ i;
      out[r] = h;
    }
}

/* { dg-final { scan-tree-dump { 1 times scalar_load costs [0-9]+ in body} "vect" } } */
/* { dg-final { scan-tree-dump-not { ([2-9]|[1-9][0-9]+) times scalar_load costs [0-9]+ in body} "vect" } } */
