// PR c++/118101
template <typename> struct A;
template <typename T> struct A<T*> {};
template <typename T> struct B { A<T*> f(); };
B<int> inst();
