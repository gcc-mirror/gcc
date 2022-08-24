/* PR tree-optimization/102645 */

volatile int a[1], d;
int b, c;

int
main ()
{
  if (b && c)
    for (c = 0; c < 3; c++)
      {
	for (int e = 0; e < 6; e++)
	  for (int f = 0; f < 12; f++)
	    d && a[0];
	continue;
      }
  return 0;
}
