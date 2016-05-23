/* This file contains input data static definitions, shared by most of
   the tests.  */

#include <arm_neon.h>
#include "arm-neon-ref.h"

/* Initialization helpers; 4 slices are needed for vld2, vld3 and
   vld4.  */
#define MY_INIT_TABLE(T,W,N) xNAME(INIT_TABLE,N)(T##W##_t)
#define MY_INIT_TABLE2(T,W,N) xNAME(INIT_TABLE2,N)(T##W##_t)
#define MY_INIT_TABLE3(T,W,N) xNAME(INIT_TABLE3,N)(T##W##_t)
#define MY_INIT_TABLE4(T,W,N) xNAME(INIT_TABLE4,N)(T##W##_t)

/* Initialized input buffers.  */
#define VECT_VAR_DECL_INIT(V, T, W, N)			\
  VECT_VAR_DECL(V,T,W,N) [] = { MY_INIT_TABLE(T,W,N) }

/* Specialized initializer with 4 entries, as used by vldX_dup and
   vdup tests, which iterate 4 times on input buffers.  */
#define VECT_VAR_DECL_INIT4(V, T, W, N)			\
  VECT_VAR_DECL(V,T,W,N) [] = { MY_INIT_TABLE(T,W,4) };

/* Initializers for arrays of vectors.  */
#define VECT_ARRAY_INIT2(V, T, W, N)		\
  T##W##_t VECT_ARRAY_VAR(V,T,W,N,2)[] =	\
  { MY_INIT_TABLE(T,W,N)			\
    MY_INIT_TABLE2(T,W,N) }

#define VECT_ARRAY_INIT3(V, T, W, N)			\
  T##W##_t VECT_ARRAY_VAR(V,T,W,N,3)[] =		\
  { MY_INIT_TABLE(T,W,N)				\
    MY_INIT_TABLE2(T,W,N)				\
    MY_INIT_TABLE3(T,W,N) }

#define VECT_ARRAY_INIT4(V, T, W, N)			\
  T##W##_t VECT_ARRAY_VAR(V,T,W,N,4)[] =		\
  { MY_INIT_TABLE(T,W,N)				\
    MY_INIT_TABLE2(T,W,N)				\
    MY_INIT_TABLE3(T,W,N)				\
    MY_INIT_TABLE4(T,W,N) }

/* Sample initialization vectors.  */
#define INIT_TABLE_1(T)				\
  (T)-16,
#define INIT_TABLE2_1(T)			\
  (T)-15,
#define INIT_TABLE3_1(T)			\
  (T)-14,
#define INIT_TABLE4_1(T)			\
  (T)-13,

#define INIT_TABLE_2(T)				\
  (T)-16, (T)-15,
#define INIT_TABLE2_2(T)			\
  (T)-14, (T)-13,
#define INIT_TABLE3_2(T)			\
  (T)-12, (T)-11,
#define INIT_TABLE4_2(T)			\
  (T)-10, (T)-9,

/* Initializer for vld3_lane tests.  */
#define INIT_TABLE_3(T)				\
  (T)-16, (T)-15, (T)-14,

#define INIT_TABLE_4(T)				\
  (T)-16, (T)-15, (T)-14, (T)-13,
#define INIT_TABLE2_4(T)			\
  (T)-12, (T)-11, (T)-10, (T)-9,
#define INIT_TABLE3_4(T)			\
  (T)-8, (T)-7, (T)-6, (T)-5,
#define INIT_TABLE4_4(T)			\
  (T)-4, (T)-3, (T)-2, (T)-1,

#define INIT_TABLE_8(T)							\
  (T)-16, (T)-15, (T)-14, (T)-13, (T)-12, (T)-11, (T)-10, (T)-9,
#define INIT_TABLE2_8(T)					\
  (T)-8, (T)-7, (T)-6, (T)-5, (T)-4, (T)-3, (T)-2, (T)-1,
#define INIT_TABLE3_8(T)				\
  (T)0, (T)1, (T)2, (T)3, (T)4, (T)5, (T)6, (T)7,
#define INIT_TABLE4_8(T)				\
  (T)8, (T)9, (T)10, (T)11, (T)12, (T)13, (T)14, (T)15,

#define INIT_TABLE_16(T)						\
  (T)-16, (T)-15, (T)-14, (T)-13, (T)-12, (T)-11, (T)-10, (T)-9,	\
  (T)-8, (T)-7, (T)-6, (T)-5, (T)-4, (T)-3, (T)-2, (T)-1,
#define INIT_TABLE2_16(T)						\
  (T)0, (T)1, (T)2, (T)3, (T)4, (T)5, (T)6, (T)7,			\
  (T)8, (T)9, (T)10, (T)11, (T)12, (T)13, (T)14, (T)15,
#define INIT_TABLE3_16(T)						\
  (T)16, (T)17, (T)18, (T)19, (T)20, (T)21, (T)22, (T)23,		\
   (T)24, (T)25, (T)26, (T)27, (T)28, (T)29, (T)30, (T)31,
#define INIT_TABLE4_16(T)						\
  (T)32, (T)33, (T)34, (T)35, (T)36, (T)37, (T)38, (T)39,		\
  (T)40, (T)41, (T)42, (T)43, (T)44, (T)45, (T)46, (T)47,

/* This one is used for padding between input buffers.  */
#define PAD(V, T, W, N) char VECT_VAR(V,T,W,N)=42

/* Input buffers, one of each size.  */
/* Insert some padding to try to exhibit out of bounds accesses.  */
VECT_VAR_DECL_INIT(buffer, int, 8, 8);
PAD(buffer_pad, int, 8, 8);
VECT_VAR_DECL_INIT(buffer, int, 16, 4);
PAD(buffer_pad, int, 16, 4);
VECT_VAR_DECL_INIT(buffer, int, 32, 2);
PAD(buffer_pad, int, 32, 2);
VECT_VAR_DECL_INIT(buffer, int, 64, 1);
PAD(buffer_pad, int, 64, 1);
VECT_VAR_DECL_INIT(buffer, uint, 8, 8);
PAD(buffer_pad, uint, 8, 8);
VECT_VAR_DECL_INIT(buffer, poly, 8, 8);
PAD(buffer_pad, poly, 8, 8);
VECT_VAR_DECL_INIT(buffer, poly, 16, 4);
PAD(buffer_pad, poly, 16, 4);
VECT_VAR_DECL_INIT(buffer, uint, 16, 4);
PAD(buffer_pad, uint, 16, 4);
VECT_VAR_DECL_INIT(buffer, uint, 32, 2);
PAD(buffer_pad, uint, 32, 2);
VECT_VAR_DECL_INIT(buffer, uint, 64, 1);
PAD(buffer_pad, uint, 64, 1);
#if defined (__ARM_FEATURE_CRYPTO)
VECT_VAR_DECL_INIT(buffer, poly, 64, 1);
PAD(buffer_pad, poly, 64, 1);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
VECT_VAR_DECL_INIT(buffer, float, 16, 4);
PAD(buffer_pad, float, 16, 4);
#endif
VECT_VAR_DECL_INIT(buffer, float, 32, 2);
PAD(buffer_pad, float, 32, 2);
VECT_VAR_DECL_INIT(buffer, int, 8, 16);
PAD(buffer_pad, int, 8, 16);
VECT_VAR_DECL_INIT(buffer, int, 16, 8);
PAD(buffer_pad, int, 16, 8);
VECT_VAR_DECL_INIT(buffer, int, 32, 4);
PAD(buffer_pad, int, 32, 4);
VECT_VAR_DECL_INIT(buffer, int, 64, 2);
PAD(buffer_pad, int, 64, 2);
VECT_VAR_DECL_INIT(buffer, uint, 8, 16);
PAD(buffer_pad, uint, 8, 16);
VECT_VAR_DECL_INIT(buffer, uint, 16, 8);
PAD(buffer_pad, uint, 16, 8);
VECT_VAR_DECL_INIT(buffer, uint, 32, 4);
PAD(buffer_pad, uint, 32, 4);
VECT_VAR_DECL_INIT(buffer, uint, 64, 2);
PAD(buffer_pad, uint, 64, 2);
VECT_VAR_DECL_INIT(buffer, poly, 8, 16);
PAD(buffer_pad, poly, 8, 16);
VECT_VAR_DECL_INIT(buffer, poly, 16, 8);
PAD(buffer_pad, poly, 16, 8);
#if defined (__ARM_FEATURE_CRYPTO)
VECT_VAR_DECL_INIT(buffer, poly, 64, 2);
PAD(buffer_pad, poly, 64, 2);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
VECT_VAR_DECL_INIT(buffer, float, 16, 8);
PAD(buffer_pad, float, 16, 8);
#endif
VECT_VAR_DECL_INIT(buffer, float, 32, 4);
PAD(buffer_pad, float, 32, 4);
#ifdef __aarch64__
VECT_VAR_DECL_INIT(buffer, float, 64, 2);
PAD(buffer_pad, float, 64, 2);
#endif

/* The tests for vld1_dup and vdup expect at least 4 entries in the
   input buffer, so force 1- and 2-elements initializers to have 4
   entries (using VECT_VAR_DECL_INIT4).  */
VECT_VAR_DECL_INIT(buffer_dup, int, 8, 8);
VECT_VAR_DECL(buffer_dup_pad, int, 8, 8);
VECT_VAR_DECL_INIT(buffer_dup, int, 16, 4);
VECT_VAR_DECL(buffer_dup_pad, int, 16, 4);
VECT_VAR_DECL_INIT4(buffer_dup, int, 32, 2);
VECT_VAR_DECL(buffer_dup_pad, int, 32, 2);
VECT_VAR_DECL_INIT4(buffer_dup, int, 64, 1);
VECT_VAR_DECL(buffer_dup_pad, int, 64, 1);
VECT_VAR_DECL_INIT(buffer_dup, uint, 8, 8);
VECT_VAR_DECL(buffer_dup_pad, uint, 8, 8);
VECT_VAR_DECL_INIT(buffer_dup, uint, 16, 4);
VECT_VAR_DECL(buffer_dup_pad, uint, 16, 4);
VECT_VAR_DECL_INIT4(buffer_dup, uint, 32, 2);
VECT_VAR_DECL(buffer_dup_pad, uint, 32, 2);
VECT_VAR_DECL_INIT4(buffer_dup, uint, 64, 1);
VECT_VAR_DECL(buffer_dup_pad, uint, 64, 1);
VECT_VAR_DECL_INIT(buffer_dup, poly, 8, 8);
VECT_VAR_DECL(buffer_dup_pad, poly, 8, 8);
VECT_VAR_DECL_INIT(buffer_dup, poly, 16, 4);
VECT_VAR_DECL(buffer_dup_pad, poly, 16, 4);
#if defined (__ARM_FEATURE_CRYPTO)
VECT_VAR_DECL_INIT4(buffer_dup, poly, 64, 1);
VECT_VAR_DECL(buffer_dup_pad, poly, 64, 1);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
VECT_VAR_DECL_INIT4(buffer_dup, float, 16, 4);
VECT_VAR_DECL(buffer_dup_pad, float, 16, 4);
#endif
VECT_VAR_DECL_INIT4(buffer_dup, float, 32, 2);
VECT_VAR_DECL(buffer_dup_pad, float, 32, 2);

VECT_VAR_DECL_INIT(buffer_dup, int, 8, 16);
VECT_VAR_DECL(buffer_dup_pad, int, 8, 16);
VECT_VAR_DECL_INIT(buffer_dup, int, 16, 8);
VECT_VAR_DECL(buffer_dup_pad, int, 16, 8);
VECT_VAR_DECL_INIT(buffer_dup, int, 32, 4);
VECT_VAR_DECL(buffer_dup_pad, int, 32, 4);
VECT_VAR_DECL_INIT4(buffer_dup, int, 64, 2);
VECT_VAR_DECL(buffer_dup_pad, int, 64, 2);
VECT_VAR_DECL_INIT(buffer_dup, uint, 8, 16);
VECT_VAR_DECL(buffer_dup_pad, uint, 8, 16);
VECT_VAR_DECL_INIT(buffer_dup, uint, 16, 8);
VECT_VAR_DECL(buffer_dup_pad, uint, 16, 8);
VECT_VAR_DECL_INIT(buffer_dup, uint, 32, 4);
VECT_VAR_DECL(buffer_dup_pad, uint, 32, 4);
VECT_VAR_DECL_INIT4(buffer_dup, uint, 64, 2);
VECT_VAR_DECL(buffer_dup_pad, uint, 64, 2);
VECT_VAR_DECL_INIT(buffer_dup, poly, 8, 16);
VECT_VAR_DECL(buffer_dup_pad, poly, 8, 16);
VECT_VAR_DECL_INIT(buffer_dup, poly, 16, 8);
VECT_VAR_DECL(buffer_dup_pad, poly, 16, 8);
#if defined (__ARM_FEATURE_CRYPTO)
VECT_VAR_DECL_INIT4(buffer_dup, poly, 64, 2);
VECT_VAR_DECL(buffer_dup_pad, poly, 64, 2);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
VECT_VAR_DECL_INIT(buffer_dup, float, 16, 8);
VECT_VAR_DECL(buffer_dup_pad, float, 16, 8);
#endif
VECT_VAR_DECL_INIT(buffer_dup, float, 32, 4);
VECT_VAR_DECL(buffer_dup_pad, float, 32, 4);
