// PR c++/50371
// { dg-options -std=c++0x }

template<decltype(nullptr)>
struct nt;
