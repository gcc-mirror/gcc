/* PR c++/109884 */
/* PowerPC doesn't define these as builtins, but macros expanding to
   *f128 builtins.  */
/* { dg-do compile { target { __float128 && { { c || c++11 } && { ! powerpc*-*-* } } } } } */
/* { dg-add-options __float128 } */

#ifdef __cplusplus
template <typename T, typename U>
struct is_same {
  static const bool value = false;
};

template <typename T>
struct is_same <T, T> {
  static const bool value = true;
};
#define HAS_TYPE(E, U) static_assert (is_same <decltype (E), U>::value, "")
#else
#define HAS_TYPE(E, U) _Static_assert (_Generic (E, default : 0, U : 1), "")
#endif

void
foo ()
{
  __float128 a = 0;
  HAS_TYPE (__builtin_infq (), __float128);
  HAS_TYPE (__builtin_huge_valq (), __float128);
  HAS_TYPE (__builtin_nanq (""), __float128);
  HAS_TYPE (__builtin_nansq (""), __float128);
  HAS_TYPE (__builtin_fabsq (a), __float128);
  HAS_TYPE (__builtin_copysignq (a, a), __float128);
}
