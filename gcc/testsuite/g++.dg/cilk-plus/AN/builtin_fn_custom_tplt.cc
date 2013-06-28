/* { dg-do run } */
/* { dg-options "-fcilkplus " } */

#if HAVE_IO 
#include <cstdio>
#endif

#include <cstdlib>

template <class T>
T my_func (T x, T y)
{
  if (x > y)
    return x;
  else
    return y;
}

template <class T>
T main_func (T  *array, T *array2, T identity_val, int size)
{
  T result;

  result = __sec_reduce (identity_val, array[0:size:1] * array2[0:size:1],
			 my_func); // my_func (identity_val, array[5] * array2[5]);
  return result;
}
int main (void)
{
  int    i_index = 0, f_index = 0, d_index = 0, l_index = 0;
  int    iarray[10], iarray2[10], i_result, i_max;
  long   larray[10], larray2[10], l_result, l_max;
  float  farray[10], farray2[10], f_result, f_max;
  double darray[10], darray2[10], d_result, d_max;
  for (int ii = 0; ii < 10; ii++)
    {
      if (ii%2 && ii)
	{
	  darray[ii] = (double)(1.0000/(double)ii);
	  farray[ii] = (float)(1.00/(float)ii);
	}
      else
	{
	  darray[ii] = (double) ii + 0.10;
	  farray[ii] = (float) (1.00/((float)(ii+1.000)));
	}
      darray2[ii] = (double) (1.00000/ (double)(ii+1));
      farray2[ii] = (float) (1.00/ (float)(ii+1));
    }

  for (int ii = 0; ii < 10; ii++)
    {
      iarray[ii] = ii;
      larray[ii] = (long)ii;
    }

  for (int ii = 0; ii < 10; ii++)
    {
      iarray2[ii] = (ii-5);
      larray2[ii] = long (ii-5);
    }
#if HAVE_IO
  printf("Int: ");
  for (int ii=0; ii < 10; ii++)
    {
      printf("%2d ", iarray[ii] * iarray2[ii]);
    }
  printf("\nfloat: ");
  for (int ii=0; ii < 10; ii++)
    {
      printf("%4.3f ", farray[ii] * farray2[ii]);
    }

  printf("\nlong: ");
  for (int ii=0; ii < 10; ii++)
    {
      printf("%2d ", larray[ii] * larray2[ii]);
    }

  printf("\ndouble: ");
  for (int ii=0; ii < 10; ii++)
    {
      printf("%4.3f ", (float) (darray[ii] * darray2[ii]));
    }
  printf("\n");
#endif

  i_result = main_func<int> (iarray, iarray2, iarray[0] * iarray2[0], 10);
  f_result = main_func<float>(farray, farray2, 0.00, 10);
  d_result = main_func<double>(darray, darray2, 0.0000, 10);
  l_result = main_func<long>(larray, larray2, 0, 10);

#if HAVE_IO
  printf("int result    = %2d\n", i_result);
  printf ("long result   = %2d\n", l_result);
  printf("float result  = %4.3f\n", f_result);
  printf("double result = %4.3lf\n", d_result);
#endif
    
  i_max = iarray[0] * iarray2[0];
  f_max = farray[0] * farray2[0];
  d_max = darray[0] * darray2[0];
  l_max = larray[0] * larray2[0];
  for (int ii = 0; ii < 10; ii++)
    {
      if (i_max < iarray[ii] * iarray2[ii])
	i_max = iarray[ii] * iarray2[ii];
      if (f_max < farray[ii] * farray2[ii])
	f_max = farray[ii] * farray2[ii];
      if (d_max < darray[ii] * darray2[ii])
	d_max = darray[ii] * darray2[ii];
      if (l_max < larray[ii] * larray2[ii])
	l_max = larray[ii] * larray2[ii];
    }

  if (i_max != i_result)
    return 1;
  if (f_max != f_result)
    return 2;
  if (d_max != d_result)
    return 3;
  if (l_max != l_result)
    return 4;
  return 0;
}

