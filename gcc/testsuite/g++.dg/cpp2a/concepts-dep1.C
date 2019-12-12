// { dg-do compile { target concepts } }

template <class T> concept True = true;
template <class T, int I = static_cast<int>(True<T>)> struct A { };
template <class T> struct B: A<T> { };
