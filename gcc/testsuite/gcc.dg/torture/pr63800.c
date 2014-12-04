/* { dg-do compile } */

int a, b, c, d[2];

int
fn1 ()
{
  int f = 0;
  d[1] = b = 1; 
  for (; b; b--)
    {
      for (c = 0; c < 2; c++)
	{
	  d[b] & 1 & b;
	  if (d[0])
	    f = d[b] * a;
	  if (f)
	    return 0;
	}
      d[b] && (d[0] = 0);
    }
  return 0;
}
