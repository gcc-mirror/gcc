/* Test diagnostic for empty declarations in old-style parameter
   declarations.  Test with no special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */

void
f (a, b)
     int; /* { dg-warning "warning: empty declaration" } */
     register; /* { dg-warning "warning: empty declaration" } */
{
}
