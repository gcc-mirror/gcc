/* Check that overloaded builtins can be used in templates with SFINAE.  */
// { dg-do compile { target c++17 } }

/* Checks performed here:
   Too few arguments (all atomic functions require more than one argument).  */
#define SFINAE_TYPE_CHECK(NAME, PARAMS, NONPOINTER_PARAMS) \
  template <typename T, typename = void> \
  struct is_##NAME##_available : std::false_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME (std::declval<T>())) >> \
    : std::true_type {};


/* Everything fails with too few arguments.  */
#define MAKE_ATOMIC_ASSERT(NAME, TYPE, SUCCESS) \
  static_assert(is_##NAME##_available<TYPE>::value == false);

#include "builtin-atomic-overloads.def"
