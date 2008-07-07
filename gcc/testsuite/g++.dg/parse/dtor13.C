/* PR c++/34963 This used to ICE */
/* { dg-do "compile" } */

struct A
{
  static friend A::~A(); /* { dg-error "storage class specifiers|extra qualification|implicitly friend" } */
};

