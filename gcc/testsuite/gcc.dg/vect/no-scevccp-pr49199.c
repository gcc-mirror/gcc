/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int const_bar (void) __attribute__ ((__const__));
int pure_bar (void) __attribute__ ((__pure__));

int foo (void)
{
  int i = 0, x = 0;
  for (; i < 100; i++)
    {
	x += const_bar ();
	x += pure_bar ();
    }
  return x;
}

