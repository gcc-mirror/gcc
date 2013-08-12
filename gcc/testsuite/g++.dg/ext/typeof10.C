// PR c++/20552
// Origin: Ivan Godard <igodard@pacbell.net>

template<int> struct A
{
  void foo()
  {
    typedef int T;                // { dg-message "previous" }
    typedef __typeof__(*this) T;  // { dg-error "conflicting" }
  }
};
