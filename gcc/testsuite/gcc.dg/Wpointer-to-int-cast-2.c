/* Test -Wpointer-to-int-cast.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wpointer-to-int-cast" } */

void *p;

char
f (void)
{
  return (char) p; /* { dg-warning "warning: cast from pointer to integer of different size" } */
}
