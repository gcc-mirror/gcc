// PR c++/116071
// { dg-options "-std=c++14 -fconcepts" }

template<class T> struct S { ~S(); };
template<class T> S<T>::~S() { }

template<typename MemPtr>
struct result_of;

template<typename Res, typename Class>
struct result_of<Res Class::*>
{
  using type = void;
};

struct thread {
  void join() { };
};
