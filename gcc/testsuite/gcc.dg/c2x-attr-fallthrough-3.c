/* Test C23 attribute syntax.  Invalid use of fallthrough attribute
   outside switch.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wextra" } */

int
f (int a)
{
  [[fallthrough]]; /* { dg-error "invalid use of attribute 'fallthrough'" } */
  return a;
}
