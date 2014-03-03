// PR c++/60065
// { dg-do compile }
// { dg-options "-std=c++1y" }

template <int> void foo(auto... x);
template <typename> void foo2(auto... x);
template <int> void foo3(auto... x, auto y, auto... z);
template <typename> void foo4(auto... x, auto y, auto... z);
