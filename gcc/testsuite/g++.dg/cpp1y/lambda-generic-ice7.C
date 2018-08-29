// PR c++/81299
// { dg-do compile { target c++14 } }
// { dg-options "-Wall" }

struct function_t {
  template <typename ...Xs>
  void operator()(Xs&& ...) const { }
};
constexpr function_t function{};

int main() {
  constexpr auto fun = ::function;
  auto call = [=](auto ...x) { fun(x...); };
  call();
}
