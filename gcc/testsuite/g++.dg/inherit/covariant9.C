// { dg-do link }
// { dg-options "-w -ansi -pedantic" }

// Contributed by Nathan Sidwell 23 Oct 2003 <nathan@codesourcery.com>
// Origin: grigory@stl.sarov.ru
// PR c++/12698. Duplicate covariant thunks emitted.

struct c1 {};

struct c0 {
  int i;
  virtual c1& f10() {}
};

struct c2 : virtual c1, c0 { };

struct c6 : virtual c2, c0 {
  virtual c2& f10() {}
};

struct c14 : virtual c2 { };

struct c19 : virtual ::c6 {
  virtual class ::c14& f10() {}
};

int main ()
{
  c19 obj;
}

  
