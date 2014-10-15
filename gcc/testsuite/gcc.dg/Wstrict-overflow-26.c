/* { dg-do compile } */
/* { dg-options "-O2 -Wstrict-overflow" } */

int
f (int i, int j)
{
  unsigned int c = 0;                                                          
  if (i < j)
    {
      unsigned int n = j - i;
      unsigned int i;
      for (i = 0; i < n; i++) /* { dg-bogus "signed overflow" } */
	c++;
    }
  return c;
}
