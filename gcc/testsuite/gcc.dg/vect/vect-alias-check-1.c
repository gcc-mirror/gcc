/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-Ofast" } */

int b, c = 1;
int a[6][5] = { {0, 0, 0, 0, 0}, {0, 0, 0, 0, 0}, {0, 1, 0, 0, 0} };

void
fn1 ()
{
  int d;
  for (b = 0; b < 5; b++)
    for (d = 4; d; d--)
      a[c + 1][b] = a[d + 1][d];
}

/* { dg-final { scan-tree-dump "improved number of alias checks from \[0-9\]* to 1" "vect" } } */
