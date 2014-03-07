// PR c++/37766
// { dg-do compile { target c++11 } }

int a = 1;
template<int& b = a> void f() {
  f<>();
}
