// Contributed by Dodji Seketeli <dodji@redhat.com>
// { dg-do compile { target c++11 } }

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
