// PR c++/86933
// { dg-do compile { target c++17 } }

template <auto... Vs> struct TT;
template <class T, class... Ms, Ms T::*... Ptrs> struct TT<Ptrs...> {};

struct X {
    int x;
    double y;
};

TT<&X::x, &X::y> t;
