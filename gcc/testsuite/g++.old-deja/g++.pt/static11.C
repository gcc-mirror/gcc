// Bug: g++ was failing to destroy C<int>::a because it was using two
// different sentry variables for construction and destruction.

extern "C" void _exit (int);

int r = 1;

struct A
{
  void f(){};
  A(){ ++r; }
  ~A(){ r -= 2; _exit (r); }
};

template<class T>
struct C
{
  C(){ a.f(); }
  static A a;
};

template <class T> A C<T>::a;
typedef C<int> B;

int main()
{
  C<int> c;
  return r;
}
