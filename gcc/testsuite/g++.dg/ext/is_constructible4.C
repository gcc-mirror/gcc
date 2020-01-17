// PR c++/93286
// { dg-do compile { target c++14 } }

struct A { static const bool value = true; };
template <bool> using __bool_constant = A;
template <typename... _Args>
struct B : __bool_constant<__is_constructible(int, _Args...)> {};
template <bool> using enable_if_t = int;
template <typename... _Args> bool is_constructible_v = B<_Args...>::value;
class C {
  template <typename _Tp, typename = enable_if_t<is_constructible_v<_Tp>>>
  C(_Tp &&);
};
using Effect_t = C;
void fn1(Effect_t effect) {
  int i;
  [](int &effect) {}(i);
}
