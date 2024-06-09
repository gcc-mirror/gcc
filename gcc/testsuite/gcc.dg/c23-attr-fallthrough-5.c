/* Test C23 fallthrough attribute: mixtures with other attributes.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

/* Use of other standard attributes together with "fallthrough" goes
   through a different path to diagnosing ignored attributes from that
   used in attribute declarations without "fallthrough".  Verify that
   such ignored attributes result in a pedwarn (for use in a context
   not permitted in the constraints for those attributes) in this case
   as well.  */

int
f (int a)
{
  switch (a)
    {
    case 1:
      a++;
      [[fallthrough, deprecated]]; /* { dg-error "attribute ignored" } */
    case 2:
      a++;
      [[maybe_unused]] [[fallthrough]]; /* { dg-error "attribute ignored" } */
    case 3:
      a++;
      [[__nodiscard__, fallthrough]]; /* { dg-error "attribute ignored" } */
    case 4:
      a++;
    }
  return a;
}
