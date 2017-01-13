/* PR77366 */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-split-paths-details" } */

void
foo(unsigned int size, unsigned int *state)
{
  unsigned int i;

  for(i = 0; i < size; i++)
    {
      if(*state & 1)
	*state ^= 1;
    }
}

/* { dg-final { scan-tree-dump-times "Duplicating join block" 0 "split-paths" } } */
