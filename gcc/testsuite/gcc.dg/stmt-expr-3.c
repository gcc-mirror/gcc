/* Test diagnostic for GNU extension: statement expressions.  Test
   with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

int
f (void)
{
  return ({ 1; }); /* { dg-error "error: ISO C forbids braced-groups within expressions" } */
}
