// { dg-do run }
// { dg-options "-pedantic" }

#include <typeinfo>

int
main (void)
{
  return typeid (__complex) != typeid (__complex double); /* { dg-warning "ISO C\\+\\+ does not support plain 'complex' meaning 'double complex'" } */
}
