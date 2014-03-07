/* PR c++/43081 */
/* { dg-do compile { target c++11 } } */

struct A
{
  typedef void (F)();
  F f = []{}; /* { dg-error "invalid pure" } */
};

struct B
{
  typedef void (F)();
  F f = 1; /* { dg-error "invalid pure" } */
  virtual F f2 = 2; /* { dg-error "invalid pure" } */
  F f3 = 3; /* { dg-error "invalid pure" } */
};
