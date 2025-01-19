/* Check that overloaded builtins can be used in templates with SFINAE.  */
// { dg-do compile { target c++17 } }

/* Checks performed here:
   Invalid parameters with various types (mismatching pointer and non-pointer
   types).  */
#define SFINAE_TYPE_CHECK(PARAMS, SHORTENED_PARAMS, INVALID_PARAMS) \
  template <typename T, typename = void> \
  struct is_available : std::false_type {}; \
  template <typename T> \
  struct is_available<T, \
    std::void_t<decltype(__builtin_speculation_safe_value INVALID_PARAMS) >> \
    : std::true_type {};

/* Mismatching pointer/non-pointer typed parameters always fail.  */
#define MAKE_SPECULATION_ASSERT(TYPE, SUCCESS) \
  static_assert(is_available<TYPE>::value == false);

#include "builtin-speculation-overloads.def"
