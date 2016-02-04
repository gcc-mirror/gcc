// PR c++/68309
// { dg-do compile { target c++11 } }

template <class... Ts> void f(Ts...);
template <class T> T g(T);
template <typename... Ts> void print(Ts... args) {
  [&] { f(g<decltype(args)>(args)...); }();
}
int main() { print(5.2); }
