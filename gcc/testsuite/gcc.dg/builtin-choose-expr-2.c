/* Test diagnostic for invalid use of __builtin_choose_expr.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

int a, b, c, d;

void
f (void)
{
  a = __builtin_choose_expr (b, c, d); /* { dg-error "first argument to '__builtin_choose_expr' not a constant" } */
}
