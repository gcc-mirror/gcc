/* PR c++/37647 */
/* { dg-do compile } */

struct A
{
  A() { void A(); } /* { dg-error "return type specification for constructor invalid|non-class scope|local external" } */
};
