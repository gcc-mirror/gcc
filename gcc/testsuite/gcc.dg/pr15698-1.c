/* Test diagnostics for old-style definition not matching prior
   prototype are present and give correct location for that prototype
   (bug 15698).  Original test.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

int	foobar ();

int func (int blah)
{
  char *rindex();
}

int foobar ()
{
  return 0;
}

char *rindex(a, b)
     register char *a, b;
{ /* { dg-warning "warning: argument 'a' doesn't match built-in prototype" } */
  return 0;
}
