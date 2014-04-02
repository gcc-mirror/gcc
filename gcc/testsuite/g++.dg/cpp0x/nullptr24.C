// PR c++/50371
// { dg-do compile { target c++11 } }

template<decltype(nullptr)>
struct nt;
