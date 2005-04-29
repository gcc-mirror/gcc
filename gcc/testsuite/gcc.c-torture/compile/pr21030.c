/* PR tree-optimization/21030
   VRP used to create invalid ranges where VR->MIN is greater than
   VR->MAX.  */

void
foo (int unit)
{
  int i;

  for (i = 0; unit; i++, unit--)
    {
      if (i >= 0)
	{
	  int j = i;
	  while (j)
	    j--;
	}
    }
}
