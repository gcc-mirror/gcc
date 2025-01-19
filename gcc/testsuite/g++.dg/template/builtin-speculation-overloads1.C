/* Check that overloaded builtins can be used in templates with SFINAE.  */
// { dg-do compile { target c++17 } }

/* Checks performed here:
   Various types (some that work, some that don't).  */
#define SFINAE_TYPE_CHECK(PARAMS, SHORTENED_PARAMS, INVALID_PARAMS) \
  template <typename T, typename = void> \
  struct is_available : std::false_type {}; \
  template <typename T> \
  struct is_available<T, \
    std::void_t<decltype(__builtin_speculation_safe_value PARAMS) >> \
    : std::true_type {};

/* Success according to type of argument.  */
#define MAKE_SPECULATION_ASSERT(TYPE, SUCCESS) \
  static_assert(is_available<TYPE>::value == SUCCESS);

#include "builtin-speculation-overloads.def"
