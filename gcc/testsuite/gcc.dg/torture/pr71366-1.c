/* { dg-do compile } */

int a[1][2], b, c;

int
fn1 ()
{ 
  int d;
  for (; c;)
    for (d = 2; d >= 0;)
      { 
	int e[4], f = e[3];
	if (f)
	  return b;
	d--;
	for (;;)
	  { 
	    c = a[0][d];
	    break;
	  }
      }
  return 0;
}
