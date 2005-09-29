// PR c++/22147

template<typename> struct A;

template<typename T> void foo(A<T>* p) { *p; }

template<typename> struct A
{
  friend void foo<class X>(A<X>*);
};

void bar()
{
  foo<int>(0);
}
