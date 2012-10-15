// PR c++/35097
// { dg-do compile { target c++11 } }

template<int> struct A;

template<> struct A<0>
{
  typedef int X [[gnu::aligned(4)]];
};

template<typename T> void foo(const A<0>::X&, T);

void bar()
{
  foo(A<0>::X(), 0);
}
