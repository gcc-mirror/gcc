/* Test diagnostic for GNU extension: omitting middle term of
   conditional expression.  Test with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

int a, b, c;

void
f (void)
{
  c = (++a ? : b); /* { dg-error "ISO C forbids omitting the middle term of a \\?: expression" } */
}
