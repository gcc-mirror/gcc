// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/11039: Implicit typename warning in friend class declaration.

template <typename T> struct X {
  struct Y {
    struct Z {};
  };
  template <typename U> friend struct Y::Z f(U);
};
