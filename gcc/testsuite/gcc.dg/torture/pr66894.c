/* { dg-do run } */

short a, b;

int
main ()
{
  for (; a != 1; a += 3)
    {
      int c = 0;
      for (; c < 2; c++)
	if (a)
	  {
	    char d = a;
	    b = d ? 1 / d : 0; 
	  }
	else
	  break;
    }
  return 0;
}
