/* Test warnings when casting from a constant integer to pointer.  
   Test with -pedantic-errors.  */
/* Origin: Carlos O'Donell <carlos@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

extern int i;
char c;
void
f (void)
{
  c = (char)&i; /* { dg-warning "warning: cast from pointer to integer of different size" } */
}
