// { dg-do compile }
// Origin: <weissr at informatik dot uni-tuebingen dot de>
// PR c++/3671: Non-type enum parameters must not be converted

enum T1 {a};
enum T2 {b};

struct Y {
  template <T1 i> void foo() {}
  template <T2 i> void foo() {}
};

struct Z {
  template <T1 i> void foo() {}
};

template void Y::foo<b> ();
template void Z::foo<b> ();   // { dg-error "" }
