/* Check that overloaded builtins can be used in templates with SFINAE.  */
// { dg-do compile { target c++17 } }
// { dg-additional-options "-Dtrue_def=true" { target speculation_barrier_defined } }
// { dg-additional-options "-Dtrue_def=false" { target { ! speculation_barrier_defined } } }

/* Checks performed here:
   Optional parameter missing works same as with optional parameter specified.  */
#define SFINAE_TYPE_CHECK(PARAMS, SHORTENED_PARAMS, INVALID_PARAMS) \
  template <typename T, typename = void> \
  struct is_available : std::false_type {}; \
  template <typename T> \
  struct is_available<T, \
    std::void_t<decltype(__builtin_speculation_safe_value SHORTENED_PARAMS) >> \
    : std::true_type {};

/* All types should fail with three arguments.  */
#define MAKE_SPECULATION_ASSERT(TYPE, SUCCESS) \
  static_assert(is_available<TYPE>::value == SUCCESS);

#include "builtin-speculation-overloads.def"

