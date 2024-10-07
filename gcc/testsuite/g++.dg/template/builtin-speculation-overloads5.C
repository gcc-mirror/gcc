/* Check that overloaded builtins still error when not in SFINAE context.  */
// { dg-do compile { target c++17 } }
#include <type_traits>

/* Checks performed here:
   Fully specified and invalid function errors before SFINAE happens.  */
template <typename T, typename = void> struct is_available : std::false_type
{
};

/* Should be error here because of the fully specified (and invalid) builtin
   call. */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } .+5 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } .+4 } */
/* { dg-error "too few arguments to function" "" { target *-*-* } .+3 } */
template <typename T>
struct is_available<
  T, std::void_t<decltype (__builtin_speculation_safe_value ())>>
  : std::true_type
{
};

/* Should be error here because of the fully specified (and invalid) builtin
   call. */
/* { dg-error "template argument 2 is invalid" "" { target *-*-* } .+5 } */
/* { dg-error "template argument 1 is invalid" "" { target *-*-* } .+4 } */
/* { dg-error "operand type 'float' is incompatible" "" { target *-*-* } .+3 } */
template <typename T>
struct is_available<
  T, std::void_t<decltype (__builtin_speculation_safe_value (float()))>>
  : std::true_type
{
};
