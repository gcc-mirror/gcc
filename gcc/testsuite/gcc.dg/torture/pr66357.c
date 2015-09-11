/* { dg-do compile } */

int a, c, e, f;
volatile int d;

void
fn1 ()
{
  if (!e)
    for (; a; a++)
      {
	if (e)
	  for (d++; d;)
	    ;
	else
	  for (c = 0; c; --c)
	    ;
	if (f)
	  for (;;)
	    {
	      if (e)
		break;
	      a = 0;
	    }
      }
}
