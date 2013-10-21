// Core 1591
// { dg-require-effective-target c++11 }

template<class T, int N> void g(T const (&)[N]);
void f() {
  g( { 1, 2, 3, 4 } );
}
