// PR c++/60384
// { dg-do compile { target c++1y } }

template<typename> int foo();

struct A
{
  typedef auto foo<>();  // { dg-error "typedef declared 'auto'" }
};
