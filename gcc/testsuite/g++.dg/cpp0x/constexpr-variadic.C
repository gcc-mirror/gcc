// { dg-options -std=c++11 }
template<class... T>
constexpr bool variadics(T&&...) { return true; }

struct IsLiteral {};

constexpr bool variadic_var = variadics(0, true, 1.2, IsLiteral{}); // Error, so below

int main() {}
