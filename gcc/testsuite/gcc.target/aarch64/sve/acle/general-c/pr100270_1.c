/* { dg-options "-msve-vector-bits=256" } */

#include <arm_sve.h>

typedef svint32_t vls_svint32_t __attribute__((arm_sve_vector_bits(256)));
typedef svuint32_t vls_svuint32_t __attribute__((arm_sve_vector_bits(256)));

typedef int32_t gnu_svint32_t __attribute__((vector_size(32)));
typedef uint32_t gnu_svuint32_t __attribute__((vector_size(32)));

#define X_gnu_svint32_t 1
#define X_gnu_svuint32_t 2
#define X_vls_svint32_t 3
#define X_vls_svuint32_t 4

#define CHECK(T) T: X_##T

#define CHECK_TYPE(EXPR, TYPE) \
  do { \
    int x[_Generic (EXPR, \
		    CHECK (gnu_svint32_t), \
		    CHECK (gnu_svuint32_t), \
		    CHECK (vls_svint32_t), \
		    CHECK (vls_svuint32_t), \
		    default : 0) == X_##TYPE ? 1 : -1]; \
  } while (0)

void
f (gnu_svint32_t sg, gnu_svuint32_t ug, vls_svint32_t sn, vls_svuint32_t un, int c)
{
  CHECK_TYPE (sg, gnu_svint32_t);
  CHECK_TYPE (ug, gnu_svuint32_t);
  CHECK_TYPE (sn, vls_svint32_t);
  CHECK_TYPE (un, vls_svuint32_t);

  CHECK_TYPE (sg + 1, gnu_svint32_t);
  CHECK_TYPE (ug + 1, gnu_svuint32_t);
  CHECK_TYPE (sn + 1, vls_svint32_t);
  CHECK_TYPE (un + 1, vls_svuint32_t);

  CHECK_TYPE (1 + sg, gnu_svint32_t);
  CHECK_TYPE (1 + ug, gnu_svuint32_t);
  CHECK_TYPE (1 + sn, vls_svint32_t);
  CHECK_TYPE (1 + un, vls_svuint32_t);

  CHECK_TYPE (sg + sg, gnu_svint32_t);
  CHECK_TYPE (ug + ug, gnu_svuint32_t);
  CHECK_TYPE (sn + sn, vls_svint32_t);
  CHECK_TYPE (un + un, vls_svuint32_t);

  /* Traditional behavior for mixed signs is to pick the signedness of the
     first operand.  We don't have any Arm-specific reason for preferring that
     behavior.  */
  CHECK_TYPE (sg + ug, gnu_svint32_t);
  CHECK_TYPE (ug + sg, gnu_svuint32_t);
  CHECK_TYPE (sn + un, vls_svint32_t);
  CHECK_TYPE (un + sn, vls_svuint32_t);

  CHECK_TYPE (c ? sg + sg : sg, gnu_svint32_t);
  CHECK_TYPE (c ? ug + ug : ug, gnu_svuint32_t);
  CHECK_TYPE (c ? sn + sn : sn, vls_svint32_t);
  CHECK_TYPE (c ? un + un : un, vls_svuint32_t);

  CHECK_TYPE (c ? sg + 1 : sg, gnu_svint32_t);
  CHECK_TYPE (c ? ug + 1 : ug, gnu_svuint32_t);
  CHECK_TYPE (c ? sn + 1 : sn, vls_svint32_t);
  CHECK_TYPE (c ? un + 1 : un, vls_svuint32_t);

  CHECK_TYPE (c ? 1 + sg : sg, gnu_svint32_t);
  CHECK_TYPE (c ? 1 + ug : ug, gnu_svuint32_t);
  CHECK_TYPE (c ? 1 + sn : sn, vls_svint32_t);
  CHECK_TYPE (c ? 1 + un : un, vls_svuint32_t);

  CHECK_TYPE (c ? sg : sg + sg, gnu_svint32_t);
  CHECK_TYPE (c ? ug : ug + ug, gnu_svuint32_t);
  CHECK_TYPE (c ? sn : sn + sn, vls_svint32_t);
  CHECK_TYPE (c ? un : un + un, vls_svuint32_t);

  CHECK_TYPE (c ? sg : sg + 1, gnu_svint32_t);
  CHECK_TYPE (c ? ug : ug + 1, gnu_svuint32_t);
  CHECK_TYPE (c ? sn : sn + 1, vls_svint32_t);
  CHECK_TYPE (c ? un : un + 1, vls_svuint32_t);

  CHECK_TYPE (c ? sg : 1 + sg, gnu_svint32_t);
  CHECK_TYPE (c ? ug : 1 + ug, gnu_svuint32_t);
  CHECK_TYPE (c ? sn : 1 + sn, vls_svint32_t);
  CHECK_TYPE (c ? un : 1 + un, vls_svuint32_t);

  CHECK_TYPE (c ? sg + sg : sg + sg, gnu_svint32_t);
  CHECK_TYPE (c ? ug + ug : ug + ug, gnu_svuint32_t);
  CHECK_TYPE (c ? sn + sn : sn + sn, vls_svint32_t);
  CHECK_TYPE (c ? un + un : un + un, vls_svuint32_t);

  CHECK_TYPE (c ? sg + sg : sg + 1, gnu_svint32_t);
  CHECK_TYPE (c ? ug + ug : ug + 1, gnu_svuint32_t);
  CHECK_TYPE (c ? sn + sn : sn + 1, vls_svint32_t);
  CHECK_TYPE (c ? un + un : un + 1, vls_svuint32_t);

  CHECK_TYPE (c ? 1 + sg : sg + sg, gnu_svint32_t);
  CHECK_TYPE (c ? 1 + ug : ug + ug, gnu_svuint32_t);
  CHECK_TYPE (c ? 1 + sn : sn + sn, vls_svint32_t);
  CHECK_TYPE (c ? 1 + un : un + un, vls_svuint32_t);
}
