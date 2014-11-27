/* { dg-do compile } */

int a, b;
void
fn1 ()
{
  int c = 0;
  while (b)
    {
      switch (c)
    case 1:
	fn1 ();
	if (a)
	  c = 1;
	b = 0;
    }
}
