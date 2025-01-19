/* Testing that we avoid error in SFINAE context when number of arguments are
   invalid in a builtin template argument.  */
// { dg-do compile { target c++17 } }
/* { dg-additional-options "-Wno-macro-redefined" { target { *-*-* } } } */
#include <type_traits>

/* Here checking the builtin_function_validate_nargs function doesn't error
   in an SFINAE context.  */

template <typename T, typename = void> struct is_available : std::false_type
{
};

/* Make one testcase for each of the functions in
   check_builtin_function_arguments that uses builtin_function_validate_nargs.
 */
#define NARGS_CHECKS(X)                                                        \
  X (constant_p, (std::declval<T> (), std::declval<T> ()))                     \
  X (isfinite, (std::declval<T> (), std::declval<T> ()))                       \
  X (isinf, (std::declval<T> (), std::declval<T> ()))                          \
  X (isinf_sign, (std::declval<T> (), std::declval<T> ()))                     \
  X (isnan, (std::declval<T> (), std::declval<T> ()))                          \
  X (isnormal, (std::declval<T> (), std::declval<T> ()))                       \
  X (issignaling, (std::declval<T> (), std::declval<T> ()))                    \
  X (signbit, (std::declval<T> (), std::declval<T> ()))                        \
  X (isgreater, (std::declval<T> ()))                                          \
  X (isgreaterequal, (std::declval<T> ()))                                     \
  X (isless, (std::declval<T> ()))                                             \
  X (islessequal, (std::declval<T> ()))                                        \
  X (islessgreater, (std::declval<T> ()))                                      \
  X (isunordered, (std::declval<T> ()))                                        \
  X (iseqsig, (std::declval<T> ()))                                            \
  X (fpclassify, (std::declval<T> ()))                                         \
  X (assume_aligned, (std::declval<T> ()))                                     \
  X (add_overflow, (std::declval<T> ()))                                       \
  X (sub_overflow, (std::declval<T> ()))                                       \
  X (mul_overflow, (std::declval<T> ()))                                       \
  X (add_overflow_p, (std::declval<T> ()))                                     \
  X (sub_overflow_p, (std::declval<T> ()))                                     \
  X (mul_overflow_p, (std::declval<T> ()))                                     \
  X (clear_padding, (std::declval<T> (), std::declval<T> ()))                  \
  X (clzg, (std::declval<T> (), std::declval<T> (), std::declval<T> ()))       \
  X (ctzg, (std::declval<T> (), std::declval<T> (), std::declval<T> ()))       \
  X (clrsbg, (std::declval<T> (), std::declval<T> (), std::declval<T> ()))     \
  X (ffsg, (std::declval<T> (), std::declval<T> (), std::declval<T> ()))       \
  X (parityg, (std::declval<T> (), std::declval<T> (), std::declval<T> ()))    \
  X (popcountg, (std::declval<T> (), std::declval<T> (), std::declval<T> ()))

#define TEMPLATE_DEFS(NAME, PARAMS)                                            \
  template <typename T, typename = void>                                       \
  struct is_##NAME##_available : std::false_type                               \
  {                                                                            \
  };                                                                           \
  template <typename T>                                                        \
  struct is_##NAME##_available<                                                \
    T, std::void_t<decltype (__builtin_##NAME PARAMS)>> : std::true_type       \
  {                                                                            \
  };

NARGS_CHECKS(TEMPLATE_DEFS)

#define MAKE_ASSERT(NAME, PARAMS) \
  static_assert(is_##NAME##_available<int>::value == false);

void foo() {
    NARGS_CHECKS(MAKE_ASSERT)
}
