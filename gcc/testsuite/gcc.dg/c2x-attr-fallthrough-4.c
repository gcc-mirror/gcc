/* Test C2x fallthrough attribute: duplicates.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

int
f (int a)
{
  switch (a)
    {
    case 1:
      a++;
      [[fallthrough, __fallthrough__]]; /* { dg-error "can appear at most once" } */
    case 2:
      a++;
      /* Separate attribute lists in the same attribute specifier
	 sequence, with the same attribute in them, are OK (but
	 receive a warning).  */
      [[fallthrough]] [[fallthrough]]; /* { dg-warning "specified multiple times" } */
    case 3:
      a++;
    }
  return a;
}
