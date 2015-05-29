// This is a copy of g++.old-deja/g++.pt/static11.C which at one
// time got a SEGV for mmix-knuth-mmixware when compiled with
// -da (or either -dj or -df).
// { dg-do compile }
// { dg-options "-da" }

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

