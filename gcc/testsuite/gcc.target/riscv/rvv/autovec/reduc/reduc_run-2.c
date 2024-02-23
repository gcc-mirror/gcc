/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable" } */

#include "reduc-2.c"

#define NROWS 53

/* -ffast-math fuzz for PLUS.  */
#define CMP__Float16(X, Y) ((X) >= (Y) * 0.875 && (X) <= (Y) * 1.125)
#define CMP_float(X, Y) ((X) == (Y))
#define CMP_double(X, Y) ((X) == (Y))
#define CMP_int8_t(X, Y) ((X) == (Y))
#define CMP_int16_t(X, Y) ((X) == (Y))
#define CMP_int32_t(X, Y) ((X) == (Y))
#define CMP_int64_t(X, Y) ((X) == (Y))
#define CMP_uint8_t(X, Y) ((X) == (Y))
#define CMP_uint16_t(X, Y) ((X) == (Y))
#define CMP_uint32_t(X, Y) ((X) == (Y))
#define CMP_uint64_t(X, Y) ((X) == (Y))

#define INIT_MATRIX(TYPE)				\
  TYPE mat[NROWS][NUM_ELEMS (TYPE)];			\
  TYPE r[NROWS];					\
  for (int i = 0; i < NROWS; i++)			\
    for (int j = 0; j < NUM_ELEMS (TYPE); j++)		\
      {							\
	mat[i][j] = i + (j * 2) * (j & 1 ? 1 : -1);	\
	asm volatile ("" ::: "memory");			\
      }

#define TEST_REDUC_PLUS(TYPE)				\
  {							\
    INIT_MATRIX (TYPE);					\
    reduc_plus_##TYPE (mat, r, NROWS);			\
    for (int i = 0; i < NROWS; i++)			\
      {							\
	volatile TYPE r2 = 0;				\
	for (int j = 0; j < NUM_ELEMS (TYPE); ++j)	\
	  r2 += mat[i][j];				\
	if (!CMP_##TYPE (r[i], r2))			\
	  __builtin_abort ();				\
      }							\
    }

#define TEST_REDUC_MAXMIN(TYPE, NAME, CMP_OP)		\
  {							\
    INIT_MATRIX (TYPE);					\
    reduc_##NAME##_##TYPE (mat, r, NROWS);		\
    for (int i = 0; i < NROWS; i++)			\
      {							\
	volatile TYPE r2 = mat[i][0];			\
	for (int j = 0; j < NUM_ELEMS (TYPE); ++j)	\
	  r2 = mat[i][j] CMP_OP r2 ? mat[i][j] : r2;	\
	if (r[i] != r2)					\
	  __builtin_abort ();				\
      }							\
    }

#define TEST_REDUC_BITWISE(TYPE, NAME, BIT_OP)		\
  {							\
    INIT_MATRIX (TYPE);					\
    reduc_##NAME##_##TYPE (mat, r, NROWS);		\
    for (int i = 0; i < NROWS; i++)			\
      {							\
	volatile TYPE r2 = mat[i][0];			\
	for (int j = 0; j < NUM_ELEMS (TYPE); ++j)	\
	  r2 BIT_OP mat[i][j];				\
	if (r[i] != r2)					\
	  __builtin_abort ();				\
      }							\
    }

int main ()
{
  TEST_PLUS (TEST_REDUC_PLUS)
  TEST_MAXMIN (TEST_REDUC_MAXMIN)

  return 0;
}
