/* { dg-do compile } */

void __assert_fail (void);

int **a, b, c, e, *j;
short *d, **f;

int *
foo ()
{
  *a = j;
  if (!(1 & e)) 
    __assert_fail ();
  return 0;
}

void
bar ()
{
  int *g = &b;
  short **h = &d;
  if ((f = &d) != h)
    for (; b;)
      {
	int i = 1;
	if (i)
	  g = foo ();
	c = 0;
      }
  if (!g)
    __assert_fail ();
}
