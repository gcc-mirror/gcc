/* PR debug/105415 */
/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fcompare-debug" } */

int m;
static int n;

void
foo (void)
{
  int s = 0;

  while (m < 1)
    {
      s += n;
      ++m;
    }
}

void
bar (int *arr, int i)
{
  while (i < 1)
    arr[i++] = 1;
}
