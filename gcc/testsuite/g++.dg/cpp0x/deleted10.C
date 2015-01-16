// PR c++/60753
// { dg-do compile { target c++11 } }

template<class T> void foo (T);

template<> void foo<int> (int);
template<> void foo<int> (int) = delete;     // { dg-error "deleted" }

struct S
{
  template<class T> void bar (T);
};

template<> void S::bar<int> (int);
template<> void S::bar<int> (int) = delete;  // { dg-error "deleted" }
