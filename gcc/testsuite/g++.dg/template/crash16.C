// { dg-do compile }
 
// Origin: Alexander Stippler  <stip@mathematik.uni-ulm.de>
// PR c++/10079

template <bool> struct A {};

template <typename> struct B
{
  enum { e };
};

template <typename T> A<(B<T>::e && 0)> foo(T) {}

template <typename T> void foo(B<T>) {}

void bar()
{
  B<int> b;
  foo(b);
}
