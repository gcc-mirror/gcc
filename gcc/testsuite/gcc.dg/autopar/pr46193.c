/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops2-details" } */

extern void abort (void);

char *
foo (int count, char **list)
{
  char *minaddr = list[0];
  int i;

  for (i = 0; i < count; i++)
    {
      char *addr = list[i];
      if (addr < minaddr)
	minaddr = addr;
    }

  return minaddr;
}

char *
foo2 (int count, char **list)
{
  char *maxaddr = list[0];
  int i;

  for (i = 0; i < count; i++)
    {
      char *addr = list[i];
      if (addr > maxaddr)
	maxaddr = addr;
    }

  return maxaddr;
}

/* { dg-final { scan-tree-dump-times "parallelizing inner loop" 2 "parloops2" } } */
