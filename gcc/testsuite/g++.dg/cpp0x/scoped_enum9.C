// DR 685 - Integral promotion of enumeration ignores fixed underlying type.
// { dg-do compile { target c++11 } }

enum E: long { e };

void f(int);
int f(long);

void g() {
  int k = f(e);
}
