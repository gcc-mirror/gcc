/* { dg-do run } */

int a[4], b, c, d; 

int
fn1 (int p)
{
  for (; d; d++)
    {
      unsigned int h;
      for (h = 0; h < 3; h++)
	{
	  if (a[c+c+h])
	    {
	      if (p)
		break;
	      return 0;
	    }
	  b = 0;
	}
    }
  return 0;
}

int
main ()
{
  fn1 (0);
  return 0;
}
