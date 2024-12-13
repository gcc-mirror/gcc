/* Testing various builtins don't complain about incorrect number of arguments.  */
// { dg-do compile { target c++17 } }
#include <type_traits>

/* Here checking the check_builtin_function_arguments function doesn't error
   in an SFINAE context.  Test each of the errors that are directly emitted
   from check_builtin_function_arguments.  */

class BasicClass { };
class Incomplete;
enum En { En_A };

/* Do not include tests against the *_overflow error message of storing to
   atomic types since I don't know how to make a type that is both TYPE_ATOMIC
   and INTEGRAL_TYPE_P in C++ (where _Atomic is not a keyword).
   Similar for clear_padding error message on _Atomic integral types.  */
#define NARGS_CHECKS(X)                                                        \
  X (clrsbg, (std::declval<T> ()), unsigned, 4)                                \
  X (ffsg, (std::declval<T> ()), unsigned, 4)                                  \
  X (clzg, (std::declval<T> ()), int, 4)                                       \
  X (ctzg, (std::declval<T> ()), int, 4)                                       \
  X (parityg, (std::declval<T> ()), int, 4)                                    \
  X (popcountg, (std::declval<T> ()), int, 4)                                  \
  X (clzg, (std::declval<T> ()), bool, 3)                                      \
  X (ctzg, (std::declval<T> ()), bool, 3)                                      \
  X (clrsbg, (std::declval<T> ()), bool, 3)                                    \
  X (ffsg, (std::declval<T> ()), bool, 3)                                      \
  X (parityg, (std::declval<T> ()), bool, 3)                                   \
  X (popcountg, (std::declval<T> ()), bool, 3)                                 \
  X (clzg, (std::declval<T> ()), En, 2)                                        \
  X (ctzg, (std::declval<T> ()), En, 2)                                        \
  X (clrsbg, (std::declval<T> ()), En, 2)                                      \
  X (ffsg, (std::declval<T> ()), En, 2)                                        \
  X (parityg, (std::declval<T> ()), En, 2)                                     \
  X (popcountg, (std::declval<T> ()), En, 2)                                   \
  X (clzg, (std::declval<T> ()), float, 1)                                     \
  X (ctzg, (std::declval<T> ()), float, 1)                                     \
  X (clrsbg, (std::declval<T> ()), float, 1)                                   \
  X (ffsg, (std::declval<T> ()), float, 1)                                     \
  X (parityg, (std::declval<T> ()), float, 1)                                  \
  X (popcountg, (std::declval<T> ()), float, 1)                                \
  X (clzg, (std::declval<T> (), std::declval<long> ()), int, 101)              \
  X (ctzg, (std::declval<T> (), std::declval<long> ()), int, 101)              \
  X (clzg, (std::declval<T> (), std::declval<T> ()), float, 100)               \
  X (ctzg, (std::declval<T> (), std::declval<T> ()), float, 100)               \
  X (clear_padding, (std::declval<T *> ()), const int, 3)                      \
  X (clear_padding, (std::declval<T *> ()), Incomplete, 2)                     \
  X (clear_padding, (std::declval<T> ()), int, 1)                              \
  X (add_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<bool> ()), int, 3)  \
  X (sub_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<bool> ()), int, 3)  \
  X (mul_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<bool> ()), int, 3)  \
  X (add_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<En> ()), int, 2)    \
  X (sub_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<En> ()), int, 2)    \
  X (mul_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<En> ()), int, 2)    \
  X (add_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<T> ()), float, 1)   \
  X (sub_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<T> ()), float, 1)   \
  X (mul_overflow_p,                                                           \
     (std::declval<T> (), std::declval<T> (), std::declval<T> ()), float, 1)   \
  X (mul_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<const int *> ()),   \
     int, 5)                                                                   \
  X (sub_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<const int *> ()),   \
     int, 5)                                                                   \
  X (add_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<const int *> ()),   \
     int, 5)                                                                   \
  X (mul_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<bool *> ()), int,   \
     4)                                                                        \
  X (sub_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<bool *> ()), int,   \
     4)                                                                        \
  X (add_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<bool *> ()), int,   \
     4)                                                                        \
  X (mul_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<En *> ()), int, 3)  \
  X (sub_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<En *> ()), int, 3)  \
  X (add_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<En *> ()), int, 3)  \
  X (mul_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<T> ()), int, 2)     \
  X (sub_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<T> ()), int, 2)     \
  X (add_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<T> ()), int, 2)     \
  X (mul_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<T *> ()), float, 1) \
  X (sub_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<T *> ()), float, 1) \
  X (add_overflow,                                                             \
     (std::declval<T> (), std::declval<T> (), std::declval<T *> ()), float, 1) \
  X (assume_aligned, (std::declval<int *> (), int (), std::declval<T> ()),     \
     float, 1)                                                                 \
  X (fpclassify,                                                               \
     (std::declval<T> (), int (), int (), int (), int (), std::declval<T> ()), \
     int, 2)                                                                   \
  X (fpclassify,                                                               \
     (std::declval<T> (), int (), int (), int (), int (), float ()), float, 1) \
  X (isgreater, (std::declval<T> (), std::declval<T> ()), int, 1)              \
  X (isgreaterequal, (std::declval<T> (), std::declval<T> ()), int, 1)         \
  X (isless, (std::declval<T> (), std::declval<T> ()), int, 1)                 \
  X (islessequal, (std::declval<T> (), std::declval<T> ()), int, 1)            \
  X (islessgreater, (std::declval<T> (), std::declval<T> ()), int, 1)          \
  X (isunordered, (std::declval<T> (), std::declval<T> ()), int, 1)            \
  X (iseqsig, (std::declval<T> (), std::declval<T> ()), int, 1)                \
  X (isinf_sign, (std::declval<T> ()), int, 1)                                 \
  X (isnan, (std::declval<T> ()), int, 1)                                      \
  X (isnormal, (std::declval<T> ()), int, 1)                                   \
  X (issignaling, (std::declval<T> ()), int, 1)                                \
  X (signbit, (std::declval<T> ()), int, 1)                                    \
  X (isinf, (std::declval<T> ()), int, 1)                                      \
  X (isfinite, (std::declval<T> ()), int, 1)                                   \
  X (alloca_with_align, (int (), int (), std::declval<T> ()), BasicClass, 1)   \
  X (alloca_with_align_and_max, (std::declval<T> (), 1), int, 1)

#define TEMPLATE_DEFS(NAME, PARAMS, TYPE, NUM)                                 \
  template <typename T, typename = void>                                       \
  struct is_##NAME##_available_##NUM : std::false_type                         \
  {                                                                            \
  };                                                                           \
  template <typename T>                                                        \
  struct is_##NAME##_available_##NUM<                                          \
    T, std::void_t<decltype (__builtin_##NAME PARAMS)>> : std::true_type       \
  {                                                                            \
  };

NARGS_CHECKS(TEMPLATE_DEFS)

#define MAKE_ASSERT(NAME, PARAMS, TYPE, NUM) \
  static_assert(is_##NAME##_available_##NUM<TYPE>::value == false);

void foo() {
    NARGS_CHECKS(MAKE_ASSERT)
}
