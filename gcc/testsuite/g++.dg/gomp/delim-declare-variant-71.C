/* { dg-do compile { target c++11 } } */

/* Test static_assert in variants.  */
/* Most of the tests in this file are broken and xfailed.
   See also delim-declare-variant-41.C for a simpler test case for
   the "base function cannot be resolved" sorry.  */

struct has_value_true { static constexpr bool value = true; };

template<typename T>
struct always_true {
  static constexpr bool value = true;
};

template<typename T>
void static_assert_in_variant_static_member_uninstantiated (T) { }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void static_assert_in_variant_static_member_uninstantiated (T)
{
  static_assert(T::value);
}
#pragma omp end declare variant

template<typename T>
void static_assert_in_variant_static_member_no_param_uninstantiated () { }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void static_assert_in_variant_static_member_no_param_uninstantiated ()
{
  static_assert(T::value);
}
#pragma omp end declare variant

template<typename T>
void static_assert_in_variant_static_member (T) { }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void static_assert_in_variant_static_member (T)
{
  static_assert(T::value);
}
#pragma omp end declare variant

template<typename T>
void static_assert_in_variant_static_member_no_param () { }  // { dg-bogus "no matching function for call" "" { xfail *-*-* } }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void static_assert_in_variant_static_member_no_param ()
{
  static_assert(T::value);
}
#pragma omp end declare variant

void instantiate_static_assert_in_variant_static_member()
{
  static_assert_in_variant_static_member(has_value_true{});
  static_assert_in_variant_static_member_no_param<has_value_true>();
}


template<typename T>
void static_assert_in_variant_templ_member_uninstantiated (T) { }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void static_assert_in_variant_templ_member_uninstantiated (T)
{
  static_assert(always_true<T>::value);
}
#pragma omp end declare variant

template<typename T>
void static_assert_in_variant_templ_member_no_param_uninstantiated () { }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void static_assert_in_variant_templ_member_no_param_uninstantiated ()
{
  static_assert(always_true<T>::value);
}
#pragma omp end declare variant

template<typename T>
void static_assert_in_variant_templ_member (T) { }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void static_assert_in_variant_templ_member (T)
{
  static_assert(always_true<T>::value);
}
#pragma omp end declare variant

template<typename T>
void static_assert_in_variant_templ_member_no_param () { }  // { dg-bogus "no matching function for call" "" { xfail *-*-* } }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void static_assert_in_variant_templ_member_no_param ()
{
  static_assert(always_true<T>::value);
}
#pragma omp end declare variant

void instantiate_static_assert_in_variant_templ_member()
{
  static_assert_in_variant_templ_member(0);
  static_assert_in_variant_templ_member_no_param<int>();
}


/* PR118530 affects also the non-delimited form of "declare variant".  */
template<bool B>
void static_assert_in_variant_nttp_uninstantiated () { }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<bool B>
void static_assert_in_variant_nttp_uninstantiated () {
  static_assert(B);
}
#pragma omp end declare variant

template<bool B>
void static_assert_in_variant_nttp () { }  // { dg-bogus "no matching function for call" "PR118530" { xfail *-*-* } }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<bool B>
void static_assert_in_variant_nttp () {
  static_assert(B);
}
#pragma omp end declare variant

void instantiate_static_assert_in_variant_nttp()
{
  static_assert_in_variant_nttp<true>();
}


template<template<typename> class Templ>
void static_assert_in_variant_template_template_uninstantiated () { }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<template<typename> class Templ>
void static_assert_in_variant_template_template_uninstantiated ()
{
  static_assert(Templ<void>::value);
}
#pragma omp end declare variant

template<template<typename> class Templ>
void static_assert_in_variant_template_template () { }  // { dg-bogus "no matching function for call" "" { xfail *-*-* } }
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<template<typename> class Templ>
void static_assert_in_variant_template_template ()
{
  static_assert(Templ<void>::value);
}
#pragma omp end declare variant

void instantiate_static_assert_in_variant_template_template()
{
  static_assert_in_variant_template_template<always_true>();
}
