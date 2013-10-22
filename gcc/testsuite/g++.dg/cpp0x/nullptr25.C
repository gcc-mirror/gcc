// { dg-options -std=c++11 }

template<decltype(nullptr)>
struct nt{};

nt<nullptr> x;
