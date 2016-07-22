/* { dg-do compile } */
/* { dg-options "-O3" } */

int a, b, c, d;

static void
fn1 ()
{
  for (b = 0; b < 1; b++)
    for (c = 0; c < 1; c++)
      {
	if (a)
	  break;
	b = 1;
      }
  for (;;)
    ;
}

int
main ()
{
  if (d)
    fn1 ();
  return 0;
}
