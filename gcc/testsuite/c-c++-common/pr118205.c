/* PR tree-optimization/118205 */

/* { dg-do compile } */
/* { dg-require-effective-target fgraphite } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O -floop-parallelize-all -ftree-parallelize-loops=2" } */

int x;
void g(int);
int f() {
  unsigned res = 0;
  int arr[] = {};
  int y = 0;
  for (unsigned int i = 1; i; i++)
    {
      for (int n = 0; n < 2; n++)
	{
	  g(arr[i]);
	  res = y > x ? y : x;
	  y = res;
        }
      g(arr[i]);
    }
  return res;
}
