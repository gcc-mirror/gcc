/* { dg-do compile } */

int a, b, c, d;

void foo ()
{
  int e;

lbl:
  for (c = 0; c < 2; c++)
    {
      e = d;
      for (; a; a++)
	{
	  d = e;
	  if (b)
	    goto lbl; 
	}
    }
}
