/* Test diagnostic for GNU extension: omitting middle term of
   conditional expression.  Test with no special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

int a, b, c;

void
f (void)
{
  c = (++a ? : b);
}
