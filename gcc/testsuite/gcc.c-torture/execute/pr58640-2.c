extern void abort (void);

int a[20], b, c; 

int
fn1 ()
{
  int d, e, f, g = 0; 

  a[12] = 1;
  for (e = 0; e < 3; e++)
    for (d = 0; d < 2; d++)
      {
	for (f = 0; f < 2; f++)
	  {
	    g ^= a[12] > 1;
	    if (g)
	      return 0;
	    if (b)
	      break;
	  }
	for (c = 0; c < 1; c++)
	  a[d] = a[e * 3 + 9]; 
      }
  return 0;
}

int
main ()
{
  fn1 ();
  if (a[0] != 0)
    abort ();
  return 0;
}
