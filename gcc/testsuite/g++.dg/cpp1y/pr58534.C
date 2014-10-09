// PR c++/58534
// { dg-do compile { target c++14 } }
// { dg-options "" }

template<typename> void foo(const auto&) {}

template<typename, typename...T> void foo(const auto&, T...) {}
