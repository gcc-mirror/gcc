// PR c++/9335
// We should not see an error about non-constant initialization.

template <int N> struct X {
    static const int value = X<N-1>::value; // { dg-error "instantiation|incomplete" }
};
template struct X<1000>;
