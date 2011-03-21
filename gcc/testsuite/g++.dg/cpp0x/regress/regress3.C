// PR c++/47511
// { dg-options -std=c++0x }

namespace N {
    template <typename T> bool g( T ) {
        return true;
    }
    struct A { };
}
template <class T> void f(const T&) {
    N::A x;
    g(x) ;
}
