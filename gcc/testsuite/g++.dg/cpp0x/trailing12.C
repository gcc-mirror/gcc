// PR c++/69139
// { dg-do compile { target c++11 } }

auto get(int) -> int { return {}; }
template <class R> int f(auto (*)(int) -> R) { return {}; }
int i = f(get);

int foo1 (auto (int) -> char);

int foo2 (auto f(int) -> char);

int foo2 (auto (f)(int) -> char);

int foo3 (auto (*f)(int) -> char);

int foo4 (auto (*const **&f)(int) -> char);

int foo5 (auto (*const **&f)(int, int *) -> char);

int foo6 (auto (int) const -> char); // { dg-error "const" }

void foo7 (auto __attribute__ ((unused)) f (int) -> int) { }
