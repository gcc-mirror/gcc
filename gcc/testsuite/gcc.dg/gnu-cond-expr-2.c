/* Test diagnostic for GNU extension: omitting middle term of
   conditional expression.  Test with -pedantic.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic" } */

int a, b, c;

void
f (void)
{
  c = (++a ? : b); /* { dg-warning "warning: ISO C forbids omitting the middle term of a \\?: expression" } */
}
