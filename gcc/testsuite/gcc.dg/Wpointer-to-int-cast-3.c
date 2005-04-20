/* Test -Wno-pointer-to-int-cast.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wno-pointer-to-int-cast" } */

void *p;

char
f (void)
{
  return (char) p;
}


char c;

void *
g (void)
{
  return (void *) c; /* { dg-warning "warning: cast to pointer from integer of different size" } */
}
