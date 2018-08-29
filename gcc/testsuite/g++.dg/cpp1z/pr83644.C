// PR c++/83644
// { dg-do compile }
// { dg-options -std=c++17 }

namespace std {
template <typename> bool is_invocable_v;
}
template <typename F> auto compose(F) {
  [](auto... objs) noexcept(std::is_invocable_v<decltype(objs)...>){};
}

auto f() { compose(3); }
