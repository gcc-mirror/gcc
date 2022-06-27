// PR c++/89197
// { dg-options "-Wno-c++11-extensions" }
// This used to ICE with std=c++03, therefore we run it in C++03 too.

template <int> void foo(int i) { const int c = int{i}; }
