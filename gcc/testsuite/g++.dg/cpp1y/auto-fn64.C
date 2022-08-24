// PR c++/105761
// { dg-do compile { target c++14 } }
// { dg-additional-options -Wno-non-template-friend }

template <class T>
class X {
  friend auto f(X);
};

struct Y : X<long> {
  friend auto f(X) { return 0L; }
};
