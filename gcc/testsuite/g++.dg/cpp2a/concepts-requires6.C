// { dg-do compile { target c++20 } }

// Test deduction requirements.

// req12.C

template <typename T, typename U>
concept SameAs = __is_same_as(T, U);

template <typename T>
concept C1 = requires(T t) { // { dg-message "in requirements" }
  { t } -> SameAs<T>; // NOTE: t deduced as decltype((t))
  // { dg-message "does not satisfy" "" { target *-*-* } .-1 }
};

template <typename T>
  requires C1<T>
constexpr bool f1() { return true; }

static_assert(f1<char>()); // { dg-error "" }
static_assert(f1<int>()); // { dg-error "" }
static_assert(f1<double>()); // { dg-error "" }


template <typename T>
concept C2 = requires(T t) {
  { t } -> SameAs<T&>; // NOTE: t deduced as decltype((t))
};

template <typename T>
  requires C2<T>
constexpr bool f2() { return true; }

static_assert(f2<int>()); // OK
