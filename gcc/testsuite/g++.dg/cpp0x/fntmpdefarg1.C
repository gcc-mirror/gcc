// PR c++/37766
// { dg-options -std=c++11 }

int a = 1;
template<int& b = a> void f() {
  f<>();
}
