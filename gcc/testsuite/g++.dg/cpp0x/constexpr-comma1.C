// PR c++/88293
// { dg-do compile { target c++11 } }

struct A
{ 
  constexpr A () { }
};

const A &a = (A (), A ());
