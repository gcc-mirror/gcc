// PR c++/50371
// { dg-options -std=c++11 }

template<decltype(nullptr)>
struct nt;
