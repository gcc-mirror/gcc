/* { dg-do compile { target rs6000-*-linux* powerpc-*-linux*} } */
/* { dg-options "-fpic -O2" } */

void f (char *s)
{
  for (;;)
    {
      int t = 6;
      switch (t)
	{
	case 2:
	  *s = '2';
	case 6: case 4: case 3: case 1:
	  break;
	}
    }
}
