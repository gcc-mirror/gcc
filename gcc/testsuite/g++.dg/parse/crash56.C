/* PR c++/43081 */
/* { dg-do compile } */
/* { dg-options "-std=c++0x" } */

struct A
{
  typedef void (F)();
  F f = []{}; /* { dg-error "invalid initializer" } */
};

struct B
{
  typedef void (F)();
  F f = 1; /* { dg-error "invalid initializer" } */
  virtual F f2 = 2; /* { dg-error "invalid initializer" } */
  F f3 = 3; /* { dg-error "invalid initializer" } */
};
