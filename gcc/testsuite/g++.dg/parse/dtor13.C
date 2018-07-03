/* PR c++/34963 This used to ICE */
/* { dg-do compile } */

struct A
{
  static friend A::~A(); /* { dg-error "3:storage class specifiers" } */
  /* { dg-error "extra qualification|implicitly friend" "" { target *-*-* } .-1 } */
};

