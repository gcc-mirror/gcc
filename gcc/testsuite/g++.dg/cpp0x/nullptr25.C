// { dg-options -std=c++0x }

template<decltype(nullptr)>
struct nt{};

nt<nullptr> x;
