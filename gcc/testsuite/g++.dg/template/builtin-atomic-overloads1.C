/* Check that overloaded builtins can be used in templates with SFINAE.  */
// { dg-do compile { target c++17 } }

/* builtin-atomic-overloads{1,2,3,4,5}.C are focussed on checking various
   properties of all the different atomic builtins.
   builtin-atomic-overloads6.C is focussed on checking all error conditions in
   the code ignoring which builtin we trigger them with.  */

/* Checks performed here:
   Correctly specified -- as long as the type is something that these builtins
   can work on.  */
#define SFINAE_TYPE_CHECK(NAME, PARAMS, NONPOINTER_PARAMS) \
  template <typename T, typename = void> \
  struct is_##NAME##_available : std::false_type {}; \
  template <typename T> \
  struct is_##NAME##_available<T, \
    std::void_t<decltype(__atomic_##NAME PARAMS) >> \
    : std::true_type {};

/* Success according to type argument.  */
#define MAKE_ATOMIC_ASSERT(NAME, TYPE, SUCCESS) \
  static_assert(is_##NAME##_available<TYPE>::value == SUCCESS);

#include "builtin-atomic-overloads.def"
