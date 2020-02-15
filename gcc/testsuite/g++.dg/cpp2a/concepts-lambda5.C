// PR c++/92556
// { dg-do compile { target c++2a } }

// Having this as a hard error is consistent with template argument deduction;
// it's an open core issue (jason 2020-02-14).
template <class T> concept has_value
  = requires { []{T::value;}; }; // { dg-error "" }
template <has_value T> void f() { }
template <class T> void f() { }
void q() { f<int>(); }
