// PR c++/60463
// PR c++/60755
// { dg-do compile { target c++11 } }
struct S {
  void f(); // { dg-message "no known conversion for implicit 'this' parameter from 'const S\\*' to 'S\\*'" }
  void g() const {
    [=] { f(); } (); // { dg-error "no matching function for call to 'S::f\\(\\)'" }
  }
};
