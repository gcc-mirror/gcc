/* { dg-do compile } */
/* { dg-additional-options "-std=c99" } */

#include <arm_neon.h>

typedef __typeof(((int32x4_t *) 0)[0][0]) int32_elt;
typedef __typeof(((uint32x4_t *) 0)[0][0]) uint32_elt;

typedef int32_elt gnu_int32x4_t __attribute__((vector_size(16)));
typedef uint32_elt gnu_uint32x4_t __attribute__((vector_size(16)));

#define X_gnu_int32x4_t 1
#define X_gnu_uint32x4_t 2
#define X_int32x4_t 3
#define X_uint32x4_t 4

#define CHECK(T) T: X_##T

#define CHECK_TYPE(EXPR, TYPE) \
  do { \
    int x[_Generic (EXPR, \
		    CHECK (gnu_int32x4_t), \
		    CHECK (gnu_uint32x4_t), \
		    CHECK (int32x4_t), \
		    CHECK (uint32x4_t), \
		    default : 0) == X_##TYPE ? 1 : -1]; \
  } while (0)

void
f (gnu_int32x4_t sg, gnu_uint32x4_t ug, int32x4_t sn, uint32x4_t un, int c)
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

  /* Traditional behavior for mixed signs is to pick the signedness of the
     first operand.  We don't have any Arm-specific reason for preferring that
     behavior, but including the tests helps to demonstrate the points in the
     comments below.  */
  CHECK_TYPE (sg + ug, gnu_int32x4_t);
  CHECK_TYPE (ug + sg, gnu_uint32x4_t);
  CHECK_TYPE (sn + un, int32x4_t);
  CHECK_TYPE (un + sn, uint32x4_t);

  /* Nothing specifies the type of mixed GNU and arm_neon.h operations, but:

     - it would be surprising if sg + un had a different signedness from
       sg + ug

     - it would also be mildly surprising if sg + un had a different type from
       both of its operands

     So in cases where the operands differ in both signedness and ABI, it seems
     more consistent to ignore the ABI difference and apply the usual rules for
     differences in sign.  */
  CHECK_TYPE (sg + un, gnu_int32x4_t);
  CHECK_TYPE (ug + sn, gnu_uint32x4_t);
  CHECK_TYPE (sn + ug, int32x4_t);
  CHECK_TYPE (un + sg, uint32x4_t);

  /* And if the first vector wins when operands differ in both signedness
     and ABI, it seems more consistent to do the same if the operands differ
     only in ABI.  */
  CHECK_TYPE (sg + sn, gnu_int32x4_t);
  CHECK_TYPE (ug + un, gnu_uint32x4_t);
  CHECK_TYPE (sn + sg, int32x4_t);
  CHECK_TYPE (un + ug, uint32x4_t);

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
