/* Test for labels in statement expressions: bugs 772 and 17913.
   switch statements must not jump into statement expressions.  */

/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (int a)
{
  switch (a)
    {
    case 0:
    case 1:
      ({
      case 2: /* { dg-error "error: case label in statement expression not containing enclosing switch statement" } */
      default: /* { dg-error "error: 'default' label in statement expression not containing enclosing switch statement" } */
	switch (a)
	  {
	  case 3:
	  default:
	    ;
	  }
	0;
      });
    }
}
