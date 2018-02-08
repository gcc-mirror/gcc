// { dg-do compile }

// Contributed by Nathan Sidwell 23 Oct 2003 <nathan@codesourcery.com>
// Origin: grigory@stl.sarov.ru
// PR c++/12700 ICE with covariancy

struct c2 { int i; };

struct c1 {
  virtual c2& f8() { static c2 a; return a; }
};

struct c3 : c1, c2 {
  virtual c2& f8() { static c2 a; return a; }
};

struct c11 : public c1 {
  virtual c3& f8() { static c3 a; return a; }
};

struct c15 : virtual c3 {
  virtual c2& f8() { static c3 a; return a; }
};

struct c18 : virtual c11 {
  virtual c15& f8();
};

c15& c18::f8() { throw 0; }
