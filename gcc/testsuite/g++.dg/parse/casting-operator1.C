// PR c++/8578
// Origin: <Delana.Lorenzo@libero.it>
// { dg-do compile }

template <typename T> struct A
{
    typedef typename T::X X;
    operator X();
};

template <typename T> A<T>::operator typename A<T>::X () {}
