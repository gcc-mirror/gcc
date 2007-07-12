/* Test diagnostic for GNU extension: statement expressions.  Test
   with -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */

int
f (void)
{
  return ({ 1; }); /* { dg-warning "ISO C forbids braced-groups within expressions" } */
}
