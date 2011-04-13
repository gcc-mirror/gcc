// PR c++/48581
// { dg-options -std=c++0x }

template<class T>
T&& create();

template<class T,
  class = decltype(foo(create<T>()))
>
auto f(int) -> char;

template<class>
auto f(...) -> char (&)[2];

struct S {};
void foo(S);

static_assert(sizeof(f<S>(0)) == 1, "Error"); // (#)

int main() {}
