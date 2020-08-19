// PR c++/93711
// { dg-do compile { target c++11 } }

struct A { A(const A&) = delete; };

A f();

struct C
{
  [[no_unique_address]] A a;
};

C c{f()};			// { dg-error "deleted" }
