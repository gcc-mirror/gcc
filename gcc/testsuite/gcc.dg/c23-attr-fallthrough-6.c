/* Test C23 attribute syntax.  Invalid use of fallthrough attribute in
   bad context inside switch.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wextra" } */

int
f (int a)
{
  switch (a)
    {
    case 1:
      a++;
      [[fallthrough]]; /* { dg-error "attribute 'fallthrough' not preceding a case label or default label" } */
      a++;
      [[fallthrough]]; /* { dg-error "attribute 'fallthrough' not preceding a case label or default label" } */
    }
  return a;
}
