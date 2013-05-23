// PR c++/57388
// { dg-require-effective-target c++11 }

template<class> struct A
{
  static constexpr bool value = false;
};

template<class Res, class... Args>
struct A<Res(Args...)>
{
  static constexpr bool value = true;
};

template<class Res, class... Args>
struct A<Res(Args...) const &>
{
  static constexpr bool value = true;
};

template<class Res, class... Args>
struct A<Res(Args...) const &&>
{
  static constexpr bool value = true;
};

static_assert(A<void()>::value, "Ouch");
static_assert(A<void() const &>::value, ""); // #1
static_assert(A<void() const &&>::value, ""); // #2
