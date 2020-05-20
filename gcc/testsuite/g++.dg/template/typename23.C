// PR c++/95223
// { dg-do compile }
// { dg-additional-options "--param=hash-table-verification-limit=10000" }

template <typename> struct j {};
template <typename t> struct n {
  typedef int m;
  j<n<t>::m> p();
};
template <typename o> j<typename n<o>::m> n<o>::p() { return o::f(); }
