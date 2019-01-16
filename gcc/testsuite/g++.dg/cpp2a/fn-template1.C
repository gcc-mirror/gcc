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
  template <class T> int f(T);
  template <class T> int g(T);
  template <class T> int h(T);
  template <class T> int e(T);
}

int v = e<N::A>(N::A());
int x = f<N::A>(N::A());
int y = g<N::A>(N::A());
int z = h<N::A>(N::A()); // { dg-error "expected" }

template<class>
void fn ()
{
  int v = e<N::A>(N::A());
  int x = f<N::A>(N::A());
  int y = g<N::A>(N::A());
  int z = h<N::A>(N::A()); // { dg-error "expected" }
}

void
test ()
{
  fn<int>();
}
