/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim1-details" } */

int r[6];

void f (int n)
{
  while (-- n)
    {
      r [0] += r [5];
      r [1] += r [0];
      r [2] += r [1];
      r [3] += r [2];
      r [4] += r [3];
      r [5] += r [4];
    }
}


/* { dg-final { scan-tree-dump-times "Executing store motion of r" 6 "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
