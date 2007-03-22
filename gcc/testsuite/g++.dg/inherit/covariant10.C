// { dg-do compile }

// Contributed by Nathan Sidwell 23 Oct 2003 <nathan@codesourcery.com>
// Origin: grigory@stl.sarov.ru
// PR c++/12699 ICE with covariancy

struct c1 {
  virtual void f1() const {}
};

struct c5 {};

struct c6 : virtual c1 {
  virtual c5* f33() const {}
};

struct c13 : virtual c5 { };

struct c17 : virtual c6
{
  virtual c13* f33() const {}
};
