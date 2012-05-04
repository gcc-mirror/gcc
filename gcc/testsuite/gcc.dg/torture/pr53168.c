/* { dg-do compile } */

int a, b, c;
unsigned *d;
int e[1];
void fn1 ();
int fn2 ();
int
fn3 ()
{
  int *f = &a;
  if (fn2 ())
    {
      for (; c; c++)
	{
	  e[a] && (b = 0);
	  fn1 ();
	  if (e[a])
	    return 0;
	}
      for (; c <= 0; c++)
	for (;;)
	  ;
    }
  else
    e[0] = 0 != (d = f);
  return *d;
}
