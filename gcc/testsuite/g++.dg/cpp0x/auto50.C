// PR c++/84348
// { dg-do compile { target c++11 } }

template<typename> struct A
{
  friend auto foo;  // { dg-error "cannot be declared friend" }
};
