/* { dg-do compile } */
/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-options "-w -O2 -mpowerpc64 -std=gnu17" } */

/*
 * (below is minimized test case)
 */

extern double pow(double x, double y);
extern long func ();
short global0, global1;
static int i0, i1, i2, i3, i4, i5;
double dbl, *array0, *array1;

void
pr64505 (short *arg0, double *arg1)
{
  int error = 0;
  short num = func (&global1 + 15, &error);
  for (int j = 0; j < array0[0]; j++)
    {
      if (j == 0)
	{
	  func (arg0, &global1 + 20, &error);
	  array0[0] = num;
	}
      else
	{
	  double cr = (&dbl)[1];
	  if (func (&i4))
	    func (0, &(&array0)[j]);
	  if (func (&i5))
	    {
	      if ((&global1)[12])
		cr = array1[j];
	    }
	  if (func (&i2)
	      && (&global1)[12])
	    {
	      if (func (&i1))
	        pow ((&dbl)[1], 2);
	    }
	    array0[j] = cr;
	}
      if (func (&i0) && global0)
        func (((short *) array0 + 1)[j]);
    }
  short ad = func (&global1 + 15, 0);
  if (func (&i3) && func ())
    *arg1 = *((double *) &global1) * ad;
  func (&global1 + 15);
}
