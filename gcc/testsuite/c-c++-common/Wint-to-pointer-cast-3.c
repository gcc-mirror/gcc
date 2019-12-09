/* Test -Wno-int-to-pointer-cast.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wno-int-to-pointer-cast" } */

char c;

void *
f (void)
{
  return (void *) c;
}

void *p;

char
g (void)
{
  return (char) p;
/* { dg-warning "10:cast from pointer to integer of different size" "" { target c } .-1 } */
/* { dg-error "10:cast from 'void\\*' to 'char' loses precision" "" { target c++ } .-2 } */
}
