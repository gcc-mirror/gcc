/* Test diagnostic for empty declarations in old-style parameter
   declarations.  Test with -pedantic-errors.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -pedantic-errors" } */

void
f (a, b)
     int; /* { dg-error "error: empty declaration" } */
     register; /* { dg-error "error: empty declaration" } */
{
}
