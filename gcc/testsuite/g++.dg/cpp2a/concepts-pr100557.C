// PR c++/100557
// { dg-do compile { target c++20 } }

template <typename _Tp> _Tp declval();

struct print_tag_;

bool tag_invoke(print_tag_, auto);
bool tag_invoke(print_tag_, auto obj) requires requires { *obj; };

template <typename CPO, typename... Args>
auto try_tag_invoke() noexcept(tag_invoke(declval<CPO>, declval<Args>()...)) // { dg-error "no matching function for call" }
    -> decltype(tag_invoke(CPO(), declval<Args>()...));

struct print_tag_ {
  void operator()(auto... args) noexcept(noexcept( try_tag_invoke<print_tag_, decltype(args)...>()));
} print;

void foo() {
  print(0);
}
