/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

double pcheck;

void foo(int n, int m, int b)
{
  int i, j;

  goto bb18;

start:
  i = 1;
  do {
    j = 1;
    do {
      double x = pcheck;
      x = x + 1;
      pcheck = x;
      j = j + 1;
    } while (j != m);
    i = i + 1;
  } while (i != n);

bb18:
  pcheck = 0.0;
  goto start;
}

/* We should have inserted two PHI nodes and the one in the i-loop
   should have 0.0 in the argument coming from the bb18 block.  */

/* { dg-final { scan-tree-dump "New PHIs: 2" "pre" } } */
/* { dg-final { scan-tree-dump "PHI <.*0\\\.0" "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
