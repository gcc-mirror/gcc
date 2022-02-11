// PR c++/101715
// { dg-do compile { target c++11 } }

template <typename T> struct S { };

template<typename T>
struct A
{
    A& foo(A&&) noexcept((S<T>::value));
    A& assign(A&&) noexcept((S<T>::value));
};
template<typename T>
A<T>& A<T>::foo(A&&) noexcept((S<T>::value)) {}
