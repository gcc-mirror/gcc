/* PR debug/105630 */
/* { dg-do compile { target pthread } } */
/* { dg-options "-O1 -ftree-parallelize-loops=2 -fcompare-debug" } */

int m;
static int n;

void
foo (void)
{
  int *arr[] = { &n, &n, &n };
  int unused = n;

  m = 0;
}

void
bar (int *arr, int i)
{
  while (i < 1)
    arr[i++] = 1;
}
