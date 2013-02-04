/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

int a[2];
int foo (int q)
{
  if (__builtin_constant_p (q))
    {
      if (q == 4)
	return a[4]; /* { dg-bogus "array subscript is above array bounds" } */
      else
	return a[0];
    }
  else
    return a[q];
}
