/* Test C2x fallthrough attribute: duplicates (allowed after N2557).  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

int
f (int a)
{
  switch (a)
    {
    case 1:
      a++;
      [[fallthrough, __fallthrough__]]; /* { dg-warning "specified multiple times" } */
    case 2:
      a++;
      [[fallthrough]] [[fallthrough]]; /* { dg-warning "specified multiple times" } */
    case 3:
      a++;
    }
  return a;
}
