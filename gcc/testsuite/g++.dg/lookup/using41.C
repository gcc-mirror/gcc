// { dg-do compile }

template <class T>
struct Base
{
    void f();
};

template <class T>
struct A : Base<T>
{
    using Base<T>::f; // { dg-message "previous declaration" }
    using Base<T>::f; // { dg-error "redeclaration" }
};

template <class T, class U>
struct B : Base<T>, Base<U>
{
    using Base<T>::f;
    using Base<U>::f;
};
