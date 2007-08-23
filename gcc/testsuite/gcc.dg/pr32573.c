/* PR tree-optimization/32573 */
/* { dg-do compile } */
/* { dg-options "-O3" } */

int
foo (void *x, long long *y)
{
  char a[256];
  int i = 0;
  long long b;
  int c;
  int d = 0;
  int e = 0;
  unsigned f = 0;
  b = bar (x);
  c = (unsigned) b;
  while (d < b && d < 65557)
    {
      f = *(unsigned *) &a[0];
      for (i = c - 4; i > 0; i--)
	if (a[i + 0] == 0x50
	    && a[i + 1] == 0x4B
	    && a[i + 3] == 0x06)
	  {
	    e = 1;
	    break;
	  }
    }
  return !e;
}
