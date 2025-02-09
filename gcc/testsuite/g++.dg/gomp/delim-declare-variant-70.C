/* { dg-do compile { target c++11 } } */

/* Check that the substituted type in variant is the same as the one in the
   base.  */

template<typename T, typename U>
struct is_same {
  static constexpr bool value = false;
};

template<typename T>
struct is_same<T, T> {
  static constexpr bool value = true;
};

/* Using static_assert directly in a variant triggers the SCOPE_REF bug noted
   in delim-declare-variant-41.C.  We'll avoid that by outsourcing the checks
   to this function.  PR118791 is a different bug that affects also the
   non-delimited form of "declare variant".  */
template<typename T, typename U>
void fail_if_not_same() {
  static_assert(is_same<T, U>::value);  // { dg-bogus "static assertion failed" "PR118791" { xfail *-*-* } }
}

/* Sanity checks are included in the base function just to be absolutely
   certain there were no mistakes made in the tests.  They should match the
   cases in the variant function exactly.  */

template<typename T>
void fwdref_passed_lvalue_int (T&& p) {
  static_assert(is_same<T, int&>::value);
  static_assert(is_same<decltype(p), int&>::value);
  static_assert(is_same<decltype((p)), int&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void fwdref_passed_lvalue_int (T&& p) {
  fail_if_not_same<T, int&>();
  fail_if_not_same<decltype(p), int&>();
  fail_if_not_same<decltype((p)), int&>();
}
#pragma omp end declare variant

template<typename T>
void fwdref_passed_lvalue_const_int (T&& p) {
  static_assert(is_same<T, int const&>::value);
  static_assert(is_same<decltype(p), int const&>::value);
  static_assert(is_same<decltype((p)), int const&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void fwdref_passed_lvalue_const_int (T&& p) {
  fail_if_not_same<T, int const&>();
  fail_if_not_same<decltype(p), int const&>();
  fail_if_not_same<decltype((p)), int const&>();
}
#pragma omp end declare variant

template<typename T>
void fwdref_passed_rvalue_int (T&& p) {
  static_assert(is_same<T, int>::value);
  static_assert(is_same<decltype(p), int&&>::value);
  static_assert(is_same<decltype((p)), int&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void fwdref_passed_rvalue_int (T&& p) {
  fail_if_not_same<T, int>();
  fail_if_not_same<decltype(p), int&&>();
  fail_if_not_same<decltype((p)), int&>();
}
#pragma omp end declare variant

template<typename T>
void fwdref_passed_rvalue_const_int (T&& p) {
  static_assert(is_same<T, int const>::value);
  static_assert(is_same<decltype(p), int const&&>::value);
  static_assert(is_same<decltype((p)), int const&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void fwdref_passed_rvalue_const_int (T&& p) {
  fail_if_not_same<T, int const>();
  fail_if_not_same<decltype(p), int const&&>();
  fail_if_not_same<decltype((p)), int const&>();
}
#pragma omp end declare variant

void instantiate_fwdref()
{
  int lvalue = 0;
  fwdref_passed_lvalue_int(lvalue);
  fwdref_passed_lvalue_const_int(static_cast<int const&>(lvalue));
  fwdref_passed_rvalue_int(0);
  fwdref_passed_rvalue_const_int(static_cast<int const&&>(0));
}



template<typename T>
void explicit_instantiate_fwdref_with_lvalue_int (T&& p) {
  static_assert(is_same<T, int&>::value);
  static_assert(is_same<decltype(p), int&>::value);
  static_assert(is_same<decltype((p)), int&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void explicit_instantiate_fwdref_with_lvalue_int (T&& p) {
  fail_if_not_same<T, int&>();
  fail_if_not_same<decltype(p), int&>();
  fail_if_not_same<decltype((p)), int&>();
}
#pragma omp end declare variant

template<typename T>
void explicit_instantiate_fwdref_with_lvalue_const_int (T&& p) {
  static_assert(is_same<T, int const&>::value);
  static_assert(is_same<decltype(p), int const&>::value);
  static_assert(is_same<decltype((p)), int const&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void explicit_instantiate_fwdref_with_lvalue_const_int (T&& p) {
  fail_if_not_same<T, int const&>();
  fail_if_not_same<decltype(p), int const&>();
  fail_if_not_same<decltype((p)), int const&>();
}
#pragma omp end declare variant

template<typename T>
void explicit_instantiate_fwdref_with_rvalue_int (T&& p) {
  static_assert(is_same<T, int&&>::value);
  static_assert(is_same<decltype(p), int&&>::value);
  static_assert(is_same<decltype((p)), int&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void explicit_instantiate_fwdref_with_rvalue_int (T&& p) {
  fail_if_not_same<T, int&&>();
  fail_if_not_same<decltype(p), int&&>();
  fail_if_not_same<decltype((p)), int&>();
}
#pragma omp end declare variant

template<typename T>
void explicit_instantiate_fwdref_with_rvalue_const_int (T&& p) {
  static_assert(is_same<T, int const&&>::value);
  static_assert(is_same<decltype(p), int const&&>::value);
  static_assert(is_same<decltype((p)), int const&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void explicit_instantiate_fwdref_with_rvalue_const_int (T&& p) {
  fail_if_not_same<T, int const&&>();
  fail_if_not_same<decltype(p), int const&&>();
  fail_if_not_same<decltype((p)), int const&>();
}
#pragma omp end declare variant

/* Technically a missuse of a forwarding reference */
void explicit_instantiate_fwdref()
{
  int lvalue = 0;
  explicit_instantiate_fwdref_with_lvalue_int<int&>(lvalue);
  explicit_instantiate_fwdref_with_lvalue_const_int<int const&>(static_cast<int const&>(lvalue));
  explicit_instantiate_fwdref_with_rvalue_int<int&&>(0);  // { dg-bogus "required from here" "PR118791" { xfail *-*-* } }
  explicit_instantiate_fwdref_with_rvalue_const_int<int const&&>(static_cast<int const&&>(0));  // { dg-bogus "required from here" "PR118791" { xfail *-*-* } }
}


template<typename T>
void const_lref_passed_lvalue_int (T const& p) {
  static_assert(is_same<T, int>::value);
  static_assert(is_same<decltype(p), int const&>::value);
  static_assert(is_same<decltype((p)), int const&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void const_lref_passed_lvalue_int (T const& p) {
  fail_if_not_same<T, int>();
  fail_if_not_same<decltype(p), int const&>();
  fail_if_not_same<decltype((p)), int const&>();
}
#pragma omp end declare variant

template<typename T>
void const_lref_passed_lvalue_const_int (T const& p) {
  static_assert(is_same<T, int>::value);
  static_assert(is_same<decltype(p), int const&>::value);
  static_assert(is_same<decltype((p)), int const&>::value);
}
#pragma omp begin declare variant match (implementation={vendor("gnu")})
template<typename T>
void const_lref_passed_lvalue_const_int (T const& p) {
  fail_if_not_same<T, int>();
  fail_if_not_same<decltype(p), int const&>();
  fail_if_not_same<decltype((p)), int const&>();
}
#pragma omp end declare variant

void instantiate_const_lref()
{
  int lvalue = 0;
  const_lref_passed_lvalue_int(lvalue);
  const_lref_passed_lvalue_const_int(static_cast<int const&>(lvalue));
}
