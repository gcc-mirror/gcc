// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/38699
// { dg-do compile }

template<class T>
struct A
{
  const T *p;
};

struct B
{
  A<int> a;
};

template class A<char>;

void
f0 ()
{
  __builtin_offsetof(A<char>, p); // OK
  __builtin_offsetof(A<char>, p[1]); // { dg-error "non constant address" }
  __builtin_offsetof(B, a.p); // OK
  __builtin_offsetof(B, a.p[1]); // { dg-error "non constant address" }
}

