// PR c++/41183
// { dg-do compile }

void foo (const char *);

template <int *>
struct A
{
  template <typename T> A (const int &, T);
  int i;
};

template <int *X>
template <typename T>
A<X>::A (const int &j, T) : i(j)
{
  foo (0);
  foo (0);
  foo (__PRETTY_FUNCTION__);
}

int N;

struct B
{
  B ();
  A<&N> a;
};

B::B() : a(N, 0) {}
