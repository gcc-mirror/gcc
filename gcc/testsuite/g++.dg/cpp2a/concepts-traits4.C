// PR c++/117294
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-diagnostics-depth=2" }

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

template <typename T> concept test_norm = norm<T>::value;  // { dg-line norm }
template <typename T> concept test_part = part<T>::value;  // { dg-line part }
template <typename T> concept test_expl = expl<T>::value;  // { dg-line expl }
template <typename T> concept test_norm_v = norm_v<T>;  // { dg-line norm_v }
template <typename T> concept test_part_v = part_v<T>;  // { dg-line part_v }
template <typename T> concept test_expl_v = expl_v<T>;  // { dg-line expl_v }

static_assert(test_norm<void>);  // { dg-error "assert" }
static_assert(test_part<void>);  // { dg-error "assert" }
static_assert(test_expl<void>);  // { dg-error "assert" }
static_assert(test_norm_v<void>);  // { dg-error "assert" }
static_assert(test_part_v<void>);  // { dg-error "assert" }
static_assert(test_expl_v<void>);  // { dg-error "assert" }
// { dg-message "'void' is not default constructible" "" { target *-*-* } norm }
// { dg-message "'void' is not default constructible" "" { target *-*-* } part }
// { dg-message "'void' is not default constructible" "" { target *-*-* } expl }
// { dg-message "'void' is not default constructible" "" { target *-*-* } norm_v }
// { dg-message "'void' is not default constructible" "" { target *-*-* } part_v }
// { dg-message "'void' is not default constructible" "" { target *-*-* } expl_v }
// { dg-prune-output "'void' is incomplete" }

static_assert(test_part<int*>);  // { dg-error "assert" }
static_assert(test_expl<int*>);  // { dg-error "assert" }
static_assert(test_part_v<int*>);  // { dg-error "assert" }
static_assert(test_expl_v<int*>);  // { dg-error "assert" }
// { dg-message ".with T = int\\*.. evaluated to .false." "" { target *-*-* } part }
// { dg-message ".with T = int\\*.. evaluated to .false." "" { target *-*-* } expl }
// { dg-message ".with T = int\\*.. evaluated to .false." "" { target *-*-* } part_v }
// { dg-message ".with T = int\\*.. evaluated to .false." "" { target *-*-* } expl_v }

static_assert(test_part<const int>);  // { dg-error "assert" }
static_assert(test_part_v<const int>);  // { dg-error "assert" }
// { dg-message "'int' is not the same as 'void'" "" { target *-*-* } part }
// { dg-message "'int' is not the same as 'void'" "" { target *-*-* } part_v }

struct S { S(int); };
static_assert(requires { requires test_norm<S>; });  // { dg-error "assert" }
static_assert(requires { requires test_part<S>; });  // { dg-error "assert" }
static_assert(requires { requires test_expl<S>; });  // { dg-error "assert" }
static_assert(requires { requires test_norm_v<S>; });  // { dg-error "assert" }
static_assert(requires { requires test_part_v<S>; });  // { dg-error "assert" }
static_assert(requires { requires test_expl_v<S>; });  // { dg-error "assert" }
// { dg-message "'S' is not default constructible" "" { target *-*-* } norm }
// { dg-message "'S' is not default constructible" "" { target *-*-* } part }
// { dg-message "'S' is not default constructible" "" { target *-*-* } expl }
// { dg-message "'S' is not default constructible" "" { target *-*-* } norm_v }
// { dg-message "'S' is not default constructible" "" { target *-*-* } part_v }
// { dg-message "'S' is not default constructible" "" { target *-*-* } expl_v }
// { dg-prune-output "no matching function for call" }
