// PR c++/60065
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

template <int> void foo(auto... x);
template <typename> void foo2(auto... x);
template <int> void foo3(auto... x, auto y, auto... z);
template <typename> void foo4(auto... x, auto y, auto... z);
