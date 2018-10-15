// PR c++/60384
// { dg-do compile { target c++14 } }

template<typename> int foo();

struct A
{
  typedef auto foo<>();  // { dg-error "11:typedef declared 'auto'" }
};
