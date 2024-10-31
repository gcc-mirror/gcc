// PR c++/113814

template <typename> struct A {};
template <typename T> A<T*> f();

template <template <typename> typename, typename> struct B;
template <template <typename> typename TT> B<TT, int> g();
