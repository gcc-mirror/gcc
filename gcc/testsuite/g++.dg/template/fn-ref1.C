// PR c++/80356

template <int (&)(int, int)> struct a;
template <int (&b)(int, int)> a<b> f();
