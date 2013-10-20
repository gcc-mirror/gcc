/* { dg-do run } */
/* { dg-additional-options "-fstrict-overflow" } */

int a, b, c, d, e;

int
main ()
{
  for (b = 4; b > -30; b--)
    for (; c;)
      for (;;)
	{
	  e = a > __INT_MAX__ - b;
	  if (d)
	    break;
	}
  return 0;
}
