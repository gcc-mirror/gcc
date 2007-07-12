/* Test -Wpointer-to-int-cast - on by default.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

void *p;

char
f (void)
{
  return (char) p; /* { dg-warning "cast from pointer to integer of different size" } */
}
