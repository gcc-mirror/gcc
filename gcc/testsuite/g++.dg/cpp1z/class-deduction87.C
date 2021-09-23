// PR c++/94102
// { dg-do compile { target c++17 } }

namespace std {
  template <typename _Tp> using decay_t = _Tp;
}
template <typename... B> struct Merged : B... {
  template <typename... T> Merged(T... t) : B(t)... {}
};
template <typename... T> Merged(T...) -> Merged<std::decay_t<T>...>;
int main() {
  auto l1 = [] {};
  auto l2 = [](int i) { return i; };
  Merged(l1, l2);
}
