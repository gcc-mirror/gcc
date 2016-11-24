/* { dg-do compile } */

int a, d, e, f, g, h;
static int b[][6] = { {0}, {0}, {1, 1}, {1} };

void
fn1 ()
{
  for (; f; f++)
    if (g)
      {
	for (e = 0; e < 5; e++)
	  if (b[e + 2][1])
	    {
	      h = b[2][e] ? 0 : a;
	      d |= 4;
	    }
	  else
	    return;
      }
}
