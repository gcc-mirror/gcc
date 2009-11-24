// { dg-do compile }
// Contributed by Wolfgang Bangerth <bangerth at ticam dot utexas dot edu>
// PR c++/509: Make sure specializations of member templates match correctly
//  between template and non-template overloads.

template <class T>
struct A {
  template <class U> void f (U);
  void f2 (int);

  template <class U> void h (U);
  void h (long);
};

template <>
struct A<float> {
  template <class U> void g (U);
  void g2 (float);
};

template <> void A<int>::f (int);                    // { dg-error "" }
// { dg-message "need 2" "" { target *-*-* } 21 }
template <> template <> void A<int>::f (int);

template <> void A<int>::f2 (int);
template <> template <> void A<int>::f2 (int);       // { dg-error "" }

template <> void A<float>::g (float);
template <> template <> void A<float>::g(float);     // { dg-error "" }

template <> void A<float>::g2 (float);               // { dg-error "" }
template <> template <> void A<float>::g2(float);    // { dg-error "" }

template <> void A<long>::h (long);
template <> template <> void A<long>::h(long);
