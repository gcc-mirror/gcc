/* Test diagnostic for GNU extension: statement expressions.  Test
   with no special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

int
f (void)
{
  return ({ 1; });
}
