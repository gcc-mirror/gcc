/* PR sanitizer/82517.  */

static int *pp;

void
baz ()
{
  return;
}

void
bar (int *p)
{
  *p = 1;
}

void
foo (int a)
{
  if (a == 2)
    {
    lab:
      baz ();
      return;
    }
  if (a > 1)
    {
      int x __attribute__ ((aligned (256)));
      pp = &x;
      bar (&x);
      if (!x)
	goto lab;
    }
}

int
main (int argc, char **argv)
{
  foo (4);
  foo (3);

  return 0;
}
