/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dse-details" } */

#define N 256

struct
{
  int x;
  int y;
} S[100];

int z[100];

int
foo (void)
{
  int i;
  int x, y;

  S[5].x = 0;
  S[5].y = 0;

  x = 5 + z[0];
  y = z[0];

  S[5].x = x;
  S[5].y = y;
}

/* { dg-final { scan-tree-dump-times "Deleted dead store" 2 "dse1" } } */
/* { dg-final { cleanup-tree-dump "dse*" } } */

