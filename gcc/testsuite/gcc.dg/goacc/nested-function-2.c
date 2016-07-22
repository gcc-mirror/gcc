/* Exercise nested function decomposition, gcc/tree-nested.c.  */

int
main (void)
{
  int j = 0, k = 6, l = 7, m = 8;
  void simple (void)
  {
    int i;
#pragma acc parallel
    {
#pragma acc loop
      for (i = 0; i < m; i+= k)
	j = (m + i - j) * l;
    }
  }
  void collapse (void)
  {
    int x, y, z;
#pragma acc parallel
    {
#pragma acc loop collapse (3)
      for (x = 0; x < k; x++)
	for (y = -5; y < l; y++)
	  for (z = 0; z < m; z++)
	    j += x + y + z;
    }
  }
  void reduction (void)
  {
    int x, y, z;
#pragma acc parallel reduction (+:j)
    {
#pragma acc loop reduction (+:j) collapse (3)
      for (x = 0; x < k; x++)
	for (y = -5; y < l; y++)
	  for (z = 0; z < m; z++)
	    j += x + y + z;
    }
  }
  simple();
  collapse();
  reduction();
  return 0;
}
