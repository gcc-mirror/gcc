// PR c++/77967
// { dg-do compile { target c++11 } }

template<typename T>
using bar = const T&;

template<typename T>
bar<T>::bar(const T& cr) // { dg-error "" }
{
}
