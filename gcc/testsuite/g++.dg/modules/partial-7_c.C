// PR c++/118101
// { dg-additional-options "-fmodules" }

template <typename> struct A {};
template <typename T> struct B { A<T*> f(); };
template struct B<int>;
import B;
