/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int printf (const char *, ...); 

unsigned a, c, *d, f;
char b, e;
short g;

void
fn1 ()
{
  unsigned h = 4294967290;
  if (b >= 0)
    {
      h = b;
      c = b / 290;
      f = ~(c - (8 || h));
      if (f)
	printf ("%d\n", 1);
      if (f)
	printf ("%d\n", f);
      g = ~f;
      if (c < 3)
	{
	  int i = -h < ~c;
	  unsigned j;
	  if (i)
	    j = h;
	  h = -j * g;
	}
      c = h;
    }
  unsigned k = ~h;
  char l = e || g;
  if (l < 1 || k < 7)
    *d = a;
}

int
main ()
{
  fn1 ();
  return 0;
}
