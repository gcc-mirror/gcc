// { dg-do compile }
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>

// PR c++/8099
// Partial specialization as friend class

template <int N, typename T> struct X;
template <typename T>        struct X<1,T>;

template <typename P> class Y {
    static int i;
    template <int N, typename T> friend struct X;
    friend struct X<1,P>;
};

template <typename T> struct X<1,T> {
    X () { Y<T>::i; }     // access private field
};
