// PR c++/123080
// { dg-do compile { target c++20 } }
template<typename T> struct type_identity { using type = T; };
auto f = [](auto x) requires requires { typename type_identity<decltype(x)>::type(0); } {};
int main() { f(0); }
