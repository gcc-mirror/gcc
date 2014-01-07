// PR c++/58534
// { dg-do compile }
// { dg-options "-std=gnu++1y" }

template<typename> void foo(const auto&) {}

template<typename, typename...T> void foo(const auto&, T...) {}
