/* { dg-do compile } */

int a, *d; 
long b;
short c;

void foo ()
{
  int e;
lbl:
  for (c = 0; c < 2; c++)
    {
      if (1 >> b)
	break;
      e = *d;
      for (; a; a++)
	{
	  *d = e;
	  if (b)
	    goto lbl;
	}
    }
}
