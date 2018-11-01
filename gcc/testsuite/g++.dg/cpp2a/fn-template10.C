// P0846R0
// { dg-do compile }
// { dg-options "-std=c++2a" }

int h;
void g();
void e();
void e(int);
void e(int, int);

namespace N {
  struct A { };
  template <class T> static int f(T) { return 1; }
  template <class T> static int g(T) { return 2; }
  template <class T> static int h(T);
  template <class T> static int e(T) { return 3; }
}

int v = e<N::A>(N::A());
int x = f<N::A>(N::A());
int y = g<N::A>(N::A());
int z = h<N::A>(N::A()); // { dg-error "expected" }
