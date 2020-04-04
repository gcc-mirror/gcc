// { dg-do compile }
// Contributed by Paul Koning <pkoning at equallogic dot com>
// PR c++/15947: Accept destructor as template in qualified-id

template <int N> struct X {
  ~X();
};

template <int N>
X<N>::~X<N>(){} // { dg-error "template-id not allowed for destructor" "" { target c++20 } }
