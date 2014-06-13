/* { dg-do run } */

int a, b;
short c, d;
char e, f;

int
fn1 (int p1, char p2)
{
  return p1 || p2 ? 0 : p2;
}

void
fn2 ()
{
  for (; a;)
    {
      int g;
      g = c = e;
      for (; a;)
	b = fn1 (g = d = e, g);
      f = g; 
    }
}

int
main ()
{
  fn2 (); 
  return 0;
}
