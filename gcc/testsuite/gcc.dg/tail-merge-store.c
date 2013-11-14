/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fdump-tree-pre" } */

int z;
int x;

void
f (int c, int d)
{
  if (c)
    z = 5;
  else
    {
      if (d)
	x = 4;
      z = 5;
    }
}

/* { dg-final { scan-tree-dump-times "duplicate of" 1 "pre"} } */
/* { dg-final { scan-tree-dump-times "z = 5" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
