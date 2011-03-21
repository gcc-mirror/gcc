// Contributed by Dodji Seketeli <dodji@redhat.com>
// { dg-options "-std=c++0x" }
// { dg-do compile }

struct F { F(int) {}};

template<class T, T* u>
struct S
{
  decltype(u) foo(T);
};

template<class T, T *u>
decltype(u) S<T, u>::foo(T)
{
  T t;
  return t;
}
