// PR c++/113629
// { dg-do compile { target c++23 } }

template <typename T> constexpr bool is_lvalue = false;
template <typename T> constexpr bool is_lvalue<T&> = true;

struct A {
  constexpr operator bool(this auto&& self) {
    return is_lvalue<decltype(self)>;
  }
};

constexpr A a;
static_assert(static_cast<bool>(a));
static_assert((bool)a);
static_assert(!static_cast<bool>(A{}));
static_assert(!(bool)A{});

struct B : A {};

constexpr B b;
static_assert(static_cast<bool>(b));
static_assert((bool)b);
static_assert(!static_cast<bool>(B{}));
static_assert(!(bool)B{});

struct C {
  template <typename R, typename T>
  explicit constexpr operator R(this T&&) {
    return is_lvalue<T>;
  }
};

constexpr C c;
static_assert(static_cast<bool>(c));
static_assert((bool)c);
static_assert(!static_cast<bool>(C{}));
static_assert(!(bool)C{});

struct D {
  explicit constexpr operator bool(this const D&) { return true; }
  explicit constexpr operator bool(this const D&&) { return false; }
};

constexpr D d;
static_assert(static_cast<bool>(d));
static_assert((bool)d);
static_assert(!static_cast<bool>(D{}));
static_assert(!(bool)D{});
