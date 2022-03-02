/* PR tree-optimization/104196 */

int a = 6;

int
main ()
{
  while (1)
    {
      int b = a < 0 && 0 < -__INT_MAX__ - a ? 0 : a;
      if (b != 4096 - __INT_MAX__)
	{
	  if (a < 6)
	    __builtin_abort ();
	  break;
	}
    }
  return 0;
}
