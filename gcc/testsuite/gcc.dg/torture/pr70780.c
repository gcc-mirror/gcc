/* { dg-do run } */

int a, b, c, *d, e;

static int
fn1 () 
{
  if (a)
    goto l1;
l2: while (b)
      if (*d)
	return c;
    for (e = 0; e; e++)
      {
	goto l2;
l1:;
      }
    return 0; 
}

int
main ()
{
  fn1 ();
  return 0; 
}
