// PR c++/117294
// { dg-do compile { target c++14 } }

template <typename T> struct norm
  { static constexpr bool value = __is_constructible(T); };
template <typename T> constexpr bool norm_v = __is_constructible(T);

template <typename T> struct part
  { static constexpr bool value = __is_constructible(T); };
template <typename T> struct part<T*>
  { static constexpr bool value = false; };
template <typename T> struct part<const T>
  { static constexpr bool value = __is_same(T, void); };
template <typename T> constexpr bool part_v = __is_constructible(T);
template <typename T> constexpr bool part_v<T*> = false;
template <typename T> constexpr bool part_v<const T> = __is_same(T, void);

template <typename T> struct expl
  { static constexpr bool value = __is_constructible(T); };
template <> struct expl<int*>
  { static constexpr bool value = false; };
template <> struct expl<const int>
  { static constexpr bool value = __is_same(int, void); };
template <typename T> constexpr bool expl_v = __is_constructible(T);
template <> constexpr bool expl_v<int*> = false;
template <> constexpr bool expl_v<const int> = __is_same(int, void);

// === Primary template can give customised diagnostics when using traits
static_assert(norm<void>::value);  // { dg-error "assert" }
// { dg-message "'void' is not default constructible" "" { target *-*-* } .-1 }
static_assert(part<void>::value);  // { dg-error "assert" }
// { dg-message "'void' is not default constructible" "" { target *-*-* } .-1 }
static_assert(expl<void>::value);  // { dg-error "assert" }
// { dg-message "'void' is not default constructible" "" { target *-*-* } .-1 }
static_assert(norm_v<void>);  // { dg-error "assert" }
// { dg-message "'void' is not default constructible" "" { target *-*-* } .-1 }
static_assert(part_v<void>);  // { dg-error "assert" }
// { dg-message "'void' is not default constructible" "" { target *-*-* } .-1 }
static_assert(expl_v<void>);  // { dg-error "assert" }
// { dg-message "'void' is not default constructible" "" { target *-*-* } .-1 }

// { dg-prune-output "'void' is incomplete" }


// === Specialisations don't customise just because primary template had trait
static_assert(part<int*>::value);  // { dg-error "assert" }
// { dg-bogus "default constructible" "" { target *-*-* } .-1 }
static_assert(expl<int*>::value);  // { dg-error "assert" }
// { dg-bogus "default constructible" "" { target *-*-* } .-1 }
static_assert(part_v<int*>);  // { dg-error "assert" }
// { dg-bogus "default constructible" "" { target *-*-* } .-1 }
static_assert(expl_v<int*>);  // { dg-error "assert" }
// { dg-bogus "default constructible" "" { target *-*-* } .-1 }


// === But partial specialisations actually using a trait can customise
static_assert(part<const int>::value);  // { dg-error "assert" }
// { dg-message "'int' is not the same as 'void'" "" { target *-*-* } .-1 }
static_assert(part_v<const int>);  // { dg-error "assert" }
// { dg-message "'int' is not the same as 'void'" "" { target *-*-* } .-1 }


// === For these cases, we no longer know that the error was caused by the trait
// === because it's been folded away before we process the failure.
static_assert(expl<const int>::value);  // { dg-error "assert" }
// { dg-bogus "because" "" { target *-*-* } .-1 }
static_assert(expl_v<const int>);  // { dg-error "assert" }
// { dg-bogus "because" "" { target *-*-* } .-1 }
static_assert(__is_constructible(void));  // { dg-error "assert" }
// { dg-bogus "because" "" { target *-*-* } .-1 }
