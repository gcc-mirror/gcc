// DR 1727: a specialization doesn't need to have the same type
// { dg-do compile { target c++14 } }

template <class T> T t = 42;
template<> void* t<int> = 0;

template<class T, class U> struct same;
template<class T> struct same<T,T> {};
same<void*,decltype(t<int>)> s;
