// PR c++/122048
// { dg-do compile { target c++14 } }

class X {
  void f();
  int i;
};
void X::f() {[&](auto) {sizeof i;}(1);}
