/* { dg-do compile } */

#include <arm_neon.h>

using int32_elt = __typeof(((int32x4_t *) nullptr)[0][0]);
using uint32_elt = __typeof(((uint32x4_t *) nullptr)[0][0]);

typedef int32_elt gnu_int32x4_t __attribute__((vector_size(16)));
typedef uint32_elt gnu_uint32x4_t __attribute__((vector_size(16)));

template<typename T> struct id;
template<> struct id<gnu_int32x4_t> { static const int value = 1; };
template<> struct id<gnu_uint32x4_t> { static const int value = 2; };
template<> struct id<int32x4_t> { static const int value = 3; };
template<> struct id<uint32x4_t> { static const int value = 4; };

#define CHECK_TYPE(EXPR, TYPE) \
  static_assert(id<decltype(EXPR)>::value == id<TYPE>::value, "foo")

void
f (gnu_int32x4_t sg, gnu_uint32x4_t ug, int32x4_t sn, uint32x4_t un, bool c)
{
  CHECK_TYPE (sg, gnu_int32x4_t);
  CHECK_TYPE (ug, gnu_uint32x4_t);
  CHECK_TYPE (sn, int32x4_t);
  CHECK_TYPE (un, uint32x4_t);

  CHECK_TYPE (sg + 1, gnu_int32x4_t);
  CHECK_TYPE (ug + 1, gnu_uint32x4_t);
  CHECK_TYPE (sn + 1, int32x4_t);
  CHECK_TYPE (un + 1, uint32x4_t);

  CHECK_TYPE (1 + sg, gnu_int32x4_t);
  CHECK_TYPE (1 + ug, gnu_uint32x4_t);
  CHECK_TYPE (1 + sn, int32x4_t);
  CHECK_TYPE (1 + un, uint32x4_t);

  CHECK_TYPE (sg + sg, gnu_int32x4_t);
  CHECK_TYPE (ug + ug, gnu_uint32x4_t);
  CHECK_TYPE (sn + sn, int32x4_t);
  CHECK_TYPE (un + un, uint32x4_t);

  // In C++, unlike C, the behavior is to prefer unsigned types over
  // signed types.
  CHECK_TYPE (sg + ug, gnu_uint32x4_t);
  CHECK_TYPE (ug + sg, gnu_uint32x4_t);
  CHECK_TYPE (sn + un, uint32x4_t);
  CHECK_TYPE (un + sn, uint32x4_t);

  // That being the case, it seems more consistent to do the same thing
  // for mixed GNU and arm_neon.h operations.
  CHECK_TYPE (sg + un, uint32x4_t);
  CHECK_TYPE (un + sg, uint32x4_t);
  CHECK_TYPE (sn + ug, gnu_uint32x4_t);
  CHECK_TYPE (ug + sn, gnu_uint32x4_t);

  // If the types have the same signedness, the traditional behavior is
  // to pick the first type if it is unsigned and the second type otherwise.
  // This is not necessarily sensible, but dates back to at least GCC 9.1.
  // We could probably change it.
  CHECK_TYPE (sg + sn, int32x4_t);
  CHECK_TYPE (sn + sg, gnu_int32x4_t);
  CHECK_TYPE (un + ug, uint32x4_t);
  CHECK_TYPE (ug + un, gnu_uint32x4_t);

  CHECK_TYPE (c ? sg + sg : sg, gnu_int32x4_t);
  CHECK_TYPE (c ? ug + ug : ug, gnu_uint32x4_t);
  CHECK_TYPE (c ? sn + sn : sn, int32x4_t);
  CHECK_TYPE (c ? un + un : un, uint32x4_t);

  CHECK_TYPE (c ? sg + 1 : sg, gnu_int32x4_t);
  CHECK_TYPE (c ? ug + 1 : ug, gnu_uint32x4_t);
  CHECK_TYPE (c ? sn + 1 : sn, int32x4_t);
  CHECK_TYPE (c ? un + 1 : un, uint32x4_t);

  CHECK_TYPE (c ? 1 + sg : sg, gnu_int32x4_t);
  CHECK_TYPE (c ? 1 + ug : ug, gnu_uint32x4_t);
  CHECK_TYPE (c ? 1 + sn : sn, int32x4_t);
  CHECK_TYPE (c ? 1 + un : un, uint32x4_t);

  CHECK_TYPE (c ? sg : sg + sg, gnu_int32x4_t);
  CHECK_TYPE (c ? ug : ug + ug, gnu_uint32x4_t);
  CHECK_TYPE (c ? sn : sn + sn, int32x4_t);
  CHECK_TYPE (c ? un : un + un, uint32x4_t);

  CHECK_TYPE (c ? sg : sg + 1, gnu_int32x4_t);
  CHECK_TYPE (c ? ug : ug + 1, gnu_uint32x4_t);
  CHECK_TYPE (c ? sn : sn + 1, int32x4_t);
  CHECK_TYPE (c ? un : un + 1, uint32x4_t);

  CHECK_TYPE (c ? sg : 1 + sg, gnu_int32x4_t);
  CHECK_TYPE (c ? ug : 1 + ug, gnu_uint32x4_t);
  CHECK_TYPE (c ? sn : 1 + sn, int32x4_t);
  CHECK_TYPE (c ? un : 1 + un, uint32x4_t);

  CHECK_TYPE (c ? sg + sg : sg + sg, gnu_int32x4_t);
  CHECK_TYPE (c ? ug + ug : ug + ug, gnu_uint32x4_t);
  CHECK_TYPE (c ? sn + sn : sn + sn, int32x4_t);
  CHECK_TYPE (c ? un + un : un + un, uint32x4_t);

  CHECK_TYPE (c ? sg + sg : sg + 1, gnu_int32x4_t);
  CHECK_TYPE (c ? ug + ug : ug + 1, gnu_uint32x4_t);
  CHECK_TYPE (c ? sn + sn : sn + 1, int32x4_t);
  CHECK_TYPE (c ? un + un : un + 1, uint32x4_t);

  CHECK_TYPE (c ? 1 + sg : sg + sg, gnu_int32x4_t);
  CHECK_TYPE (c ? 1 + ug : ug + ug, gnu_uint32x4_t);
  CHECK_TYPE (c ? 1 + sn : sn + sn, int32x4_t);
  CHECK_TYPE (c ? 1 + un : un + un, uint32x4_t);
}
