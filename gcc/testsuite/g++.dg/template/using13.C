//PR c++/28051

template<int> struct A {};

template<int N> struct B : A<N>
{
    using A<N>::operator typename A<N>::X; // { dg-error "no type named" }
};

B<0> b;

