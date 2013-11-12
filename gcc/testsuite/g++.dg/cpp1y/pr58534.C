// { dg-do compile }
// { dg-options "-std=gnu++1y" }

// PR c++/58534

template<typename> void foo(const auto&) {}

template<typename, typename...T> void foo(const auto&, T...) {}

