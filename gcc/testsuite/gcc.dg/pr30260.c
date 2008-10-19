/* PR 30260  */
/* { dg-do link } */
/* { dg-options "-pedantic -O" } */
#include <limits.h>

enum A {
  A1 = 0, 
  A2 = A1 - 1
};
enum B {
  B1 = 0u, 
  B2 = B1 - 1 /* { dg-bogus "ISO C restricts enumerator values to range of 'int'" } */
};
int main(void)
{
  enum A a = -1;
  enum B b = -1;

  if (!(a < 0))
    link_error ();
  if (!(A2 < 0))
    link_error ();
  if (!(b < 0))
    link_error ();
  if (!(B2 < 0))
    link_error ();

  return 0;
}

enum E1 { e10 = INT_MAX, e11 }; /* { dg-error "overflow in enumeration values" } */
enum E2 { e20 = (unsigned) INT_MAX, e21 }; /* { dg-error "overflow in enumeration values" } */
