/* The predicate analysis must not treat (a < b) and (a >= b) as
   complements: for a possibly NaN operand both are false, so X is left
   uninitialized on that path.
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

int
f (double a, double b)
{
  int x;
  if (a < b)
    x = 1;
  else if (a >= b)
    x = 2;
  return x;	/* { dg-warning "may be used uninitialized" } */
}
