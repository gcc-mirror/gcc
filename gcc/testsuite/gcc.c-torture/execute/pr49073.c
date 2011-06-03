/* PR tree-optimization/49073 */

extern void abort (void);
int a[] = { 1, 2, 3, 4, 5, 6, 7 }, c;

int
main ()
{
  int d = 1, i = 1;
  _Bool f = 0;
  do
    {
      d = a[i];
      if (f && d == 4)
	{
	  ++c;
	  break;
	}
      i++;
      f = (d == 3);
    }
  while (d < 7);
  if (c != 1)
    abort ();
  return 0;
}
