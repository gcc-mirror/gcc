// PR c++/69515
// { dg-do link { target c++14 } }

struct A { A(int = 0) {} };

template<class...> class meow;

template<typename T> A foo;
template<typename... Ts> A foo<meow<Ts...>> = 1;

auto&& a = foo<meow<int>>;
auto&& b = foo<meow<int, int>>;

int main() {}
