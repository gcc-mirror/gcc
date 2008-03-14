/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre-details" } */

struct
{
  int x;
  int y;
} S[100];

int z[100];

int
foo (int y)
{
  int x;

  S[5].x = 4;
  S[5].y = 0;

  x = S[5].x;

  return (x);
}

/* { dg-final { scan-tree-dump "Replaced S\\\[5\\\].x with 4" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
