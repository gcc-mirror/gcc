/* PR c++/37555 */
/* { dg-do compile } */

struct A {};

typedef void (A::T)(); /* { dg-error "typedef name may not be a nested-name-specifier" } */

void foo()
{
  T t;
  t; /* { dg-error "was not declared" } */
}
