// PR c++/42623
// We should choose f(B) because f(A<undef>) involves applying sizeof to
// an incomplete class, so it is removed by SFINAE.
// { dg-do link }

struct undef;

template <typename U, int N = sizeof(U)> struct A { A(int); };
template <typename U> void f(A<U>);

template <typename U> struct B { B(int) { } };
template <typename U> void f(B<U>) { }

int main()
{
  f<undef>(0);
}
