/* PR c++/32111 */
/* This used to ICE. */

/* { dg-do compile } */

struct A
{
  friend A::~A() {} /* { dg-error "implicitly friends of their class" } */
};
