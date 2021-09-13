// PR c++/98810
// { dg-do compile { target c++20 } }

template <auto> struct a {};
template <int i, a <i> s = a <i> {}> using b = a <s>;
template <int i> constexpr auto g (const b <i> &) { return true; };
