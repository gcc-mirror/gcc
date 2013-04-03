// { dg-require-effective-target c++11 }

template <class,class> struct ST;
template <class T> struct ST<T,T> {};

struct A
{
  int f() &;
  char f() &&;
};

template <class T> struct B
{
  int f() &;
  char f() &&;
};

int main()
{
  A a;
  a.f();
  A().f();
  ST<decltype(a.f()), int>();
  ST<decltype(A().f()), char>();
  B<int> b;
  b.f();
  B<int>().f();
  ST<decltype(b.f()), int>();
  ST<decltype(B<int>().f()), char>();
}
