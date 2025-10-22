/* { dg-do compile } */
/* { dg-additional-options "-mavx512vl -mavx512bw" { target { avx512vl && avx512bw } } } */

bool f(bool splat, bool swizzle_splat,
       int *elems, int length)
{
  int input = elems[0];
  for (int i = 0; i < length; i++)
    {
      if (input != elems[i])
	{
	  splat = false;
	  swizzle_splat = false;
	}
    }
  return (splat && swizzle_splat);
}
