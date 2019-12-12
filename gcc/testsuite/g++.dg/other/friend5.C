/* PR c++/32111 */
/* This used to ICE. */

/* { dg-do compile } */

struct A
{
  friend A::~A() {} /* { dg-error "3:member functions are implicitly friends of their class" } */
};
