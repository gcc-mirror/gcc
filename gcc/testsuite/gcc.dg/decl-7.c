/* Test diagnostic for array defaulting to one element.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

int a[]; /* { dg-warning "warning: array 'a' assumed to have one element" } */
