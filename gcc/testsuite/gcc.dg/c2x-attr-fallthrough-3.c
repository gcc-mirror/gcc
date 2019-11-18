/* Test C2x attribute syntax.  Invalid use of fallthrough attribute
   outside switch or in bad context inside switch.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wextra" } */

int
f (int a)
{
  [[fallthrough]]; /* { dg-error "invalid use of attribute 'fallthrough'" } */
  switch (a)
    {
    case 1:
      a++;
      [[fallthrough]]; /* { dg-warning "attribute 'fallthrough' not preceding a case label or default label" } */
      a++;
    }
  return a;
}
