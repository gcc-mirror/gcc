/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options { -O2 -fdump-tree-vect-details -fno-vect-cost-model } }  */

#include "stdint-gcc.h"
#include <assert.h>

#define DEF64(TYPEDST, TYPESRC)                                                \
  void __attribute__ ((noipa))                                                 \
  popcount64_##TYPEDST##TYPESRC (TYPEDST *restrict dst, TYPESRC *restrict src, \
				 int size)                                     \
  {                                                                            \
    for (int i = 0; i < size; ++i)                                             \
      dst[i] = __builtin_popcountll (src[i]);                                  \
  }

#define DEF32(TYPEDST, TYPESRC)                                                \
  void __attribute__ ((noipa))                                                 \
  popcount32_##TYPEDST##TYPESRC (TYPEDST *restrict dst, TYPESRC *restrict src, \
				 int size)                                     \
  {                                                                            \
    for (int i = 0; i < size; ++i)                                             \
      dst[i] = __builtin_popcount (src[i]);                                    \
  }

#define DEFCTZ64(TYPEDST, TYPESRC)                                             \
  void __attribute__ ((noipa))                                                 \
  ctz64_##TYPEDST##TYPESRC (TYPEDST *restrict dst, TYPESRC *restrict src,      \
			    int size)                                          \
  {                                                                            \
    for (int i = 0; i < size; ++i)                                             \
      dst[i] = __builtin_ctzll (src[i]);                                       \
  }

#define DEFCTZ32(TYPEDST, TYPESRC)                                             \
  void __attribute__ ((noipa))                                                 \
  ctz32_##TYPEDST##TYPESRC (TYPEDST *restrict dst, TYPESRC *restrict src,      \
			    int size)                                          \
  {                                                                            \
    for (int i = 0; i < size; ++i)                                             \
      dst[i] = __builtin_ctz (src[i]);                                         \
  }

#define DEFFFS64(TYPEDST, TYPESRC)                                             \
  void __attribute__ ((noipa))                                                 \
  ffs64_##TYPEDST##TYPESRC (TYPEDST *restrict dst, TYPESRC *restrict src,      \
			    int size)                                          \
  {                                                                            \
    for (int i = 0; i < size; ++i)                                             \
      dst[i] = __builtin_ffsll (src[i]);                                       \
  }

#define DEFFFS32(TYPEDST, TYPESRC)                                             \
  void __attribute__ ((noipa))                                                 \
  ffs32_##TYPEDST##TYPESRC (TYPEDST *restrict dst, TYPESRC *restrict src,      \
			    int size)                                          \
  {                                                                            \
    for (int i = 0; i < size; ++i)                                             \
      dst[i] = __builtin_ffs (src[i]);                                         \
  }

#define DEF_ALL()                                                              \
  DEF64 (uint64_t, uint64_t)                                                   \
  DEF64 (uint64_t, uint32_t)                                                   \
  DEF64 (uint64_t, uint16_t)                                                   \
  DEF64 (uint64_t, uint8_t)                                                    \
  DEF64 (uint64_t, int64_t)                                                    \
  DEF64 (uint64_t, int32_t)                                                    \
  DEF64 (uint64_t, int16_t)                                                    \
  DEF64 (uint64_t, int8_t)                                                     \
  DEF64 (int64_t, uint64_t)                                                    \
  DEF64 (int64_t, uint32_t)                                                    \
  DEF64 (int64_t, uint16_t)                                                    \
  DEF64 (int64_t, uint8_t)                                                     \
  DEF64 (int64_t, int64_t)                                                     \
  DEF64 (int64_t, int32_t)                                                     \
  DEF64 (int64_t, int16_t)                                                     \
  DEF64 (int64_t, int8_t)                                                      \
  DEF64 (uint32_t, uint64_t)                                                   \
  DEF64 (uint32_t, uint32_t)                                                   \
  DEF64 (uint32_t, uint16_t)                                                   \
  DEF64 (uint32_t, uint8_t)                                                    \
  DEF64 (uint32_t, int64_t)                                                    \
  DEF64 (uint32_t, int32_t)                                                    \
  DEF64 (uint32_t, int16_t)                                                    \
  DEF64 (uint32_t, int8_t)                                                     \
  DEF64 (int32_t, uint64_t)                                                    \
  DEF64 (int32_t, uint32_t)                                                    \
  DEF64 (int32_t, uint16_t)                                                    \
  DEF64 (int32_t, uint8_t)                                                     \
  DEF64 (int32_t, int64_t)                                                     \
  DEF64 (int32_t, int32_t)                                                     \
  DEF64 (int32_t, int16_t)                                                     \
  DEF64 (int32_t, int8_t)                                                      \
  DEF64 (uint16_t, uint64_t)                                                   \
  DEF64 (uint16_t, uint32_t)                                                   \
  DEF64 (uint16_t, uint16_t)                                                   \
  DEF64 (uint16_t, uint8_t)                                                    \
  DEF64 (uint16_t, int64_t)                                                    \
  DEF64 (uint16_t, int32_t)                                                    \
  DEF64 (uint16_t, int16_t)                                                    \
  DEF64 (uint16_t, int8_t)                                                     \
  DEF64 (int16_t, uint64_t)                                                    \
  DEF64 (int16_t, uint32_t)                                                    \
  DEF64 (int16_t, uint16_t)                                                    \
  DEF64 (int16_t, uint8_t)                                                     \
  DEF64 (int16_t, int64_t)                                                     \
  DEF64 (int16_t, int32_t)                                                     \
  DEF64 (int16_t, int16_t)                                                     \
  DEF64 (int16_t, int8_t)                                                      \
  DEF64 (uint8_t, uint64_t)                                                    \
  DEF64 (uint8_t, uint32_t)                                                    \
  DEF64 (uint8_t, uint16_t)                                                    \
  DEF64 (uint8_t, uint8_t)                                                     \
  DEF64 (uint8_t, int64_t)                                                     \
  DEF64 (uint8_t, int32_t)                                                     \
  DEF64 (uint8_t, int16_t)                                                     \
  DEF64 (uint8_t, int8_t)                                                      \
  DEF64 (int8_t, uint64_t)                                                     \
  DEF64 (int8_t, uint32_t)                                                     \
  DEF64 (int8_t, uint16_t)                                                     \
  DEF64 (int8_t, uint8_t)                                                      \
  DEF64 (int8_t, int64_t)                                                      \
  DEF64 (int8_t, int32_t)                                                      \
  DEF64 (int8_t, int16_t)                                                      \
  DEF64 (int8_t, int8_t)                                                       \
  DEF32 (uint64_t, uint64_t)                                                   \
  DEF32 (uint64_t, uint32_t)                                                   \
  DEF32 (uint64_t, uint16_t)                                                   \
  DEF32 (uint64_t, uint8_t)                                                    \
  DEF32 (uint64_t, int64_t)                                                    \
  DEF32 (uint64_t, int32_t)                                                    \
  DEF32 (uint64_t, int16_t)                                                    \
  DEF32 (uint64_t, int8_t)                                                     \
  DEF32 (int64_t, uint64_t)                                                    \
  DEF32 (int64_t, uint32_t)                                                    \
  DEF32 (int64_t, uint16_t)                                                    \
  DEF32 (int64_t, uint8_t)                                                     \
  DEF32 (int64_t, int64_t)                                                     \
  DEF32 (int64_t, int32_t)                                                     \
  DEF32 (int64_t, int16_t)                                                     \
  DEF32 (int64_t, int8_t)                                                      \
  DEF32 (uint32_t, uint64_t)                                                   \
  DEF32 (uint32_t, uint32_t)                                                   \
  DEF32 (uint32_t, uint16_t)                                                   \
  DEF32 (uint32_t, uint8_t)                                                    \
  DEF32 (uint32_t, int64_t)                                                    \
  DEF32 (uint32_t, int32_t)                                                    \
  DEF32 (uint32_t, int16_t)                                                    \
  DEF32 (uint32_t, int8_t)                                                     \
  DEF32 (int32_t, uint64_t)                                                    \
  DEF32 (int32_t, uint32_t)                                                    \
  DEF32 (int32_t, uint16_t)                                                    \
  DEF32 (int32_t, uint8_t)                                                     \
  DEF32 (int32_t, int64_t)                                                     \
  DEF32 (int32_t, int32_t)                                                     \
  DEF32 (int32_t, int16_t)                                                     \
  DEF32 (int32_t, int8_t)                                                      \
  DEF32 (uint16_t, uint64_t)                                                   \
  DEF32 (uint16_t, uint32_t)                                                   \
  DEF32 (uint16_t, uint16_t)                                                   \
  DEF32 (uint16_t, uint8_t)                                                    \
  DEF32 (uint16_t, int64_t)                                                    \
  DEF32 (uint16_t, int32_t)                                                    \
  DEF32 (uint16_t, int16_t)                                                    \
  DEF32 (uint16_t, int8_t)                                                     \
  DEF32 (int16_t, uint64_t)                                                    \
  DEF32 (int16_t, uint32_t)                                                    \
  DEF32 (int16_t, uint16_t)                                                    \
  DEF32 (int16_t, uint8_t)                                                     \
  DEF32 (int16_t, int64_t)                                                     \
  DEF32 (int16_t, int32_t)                                                     \
  DEF32 (int16_t, int16_t)                                                     \
  DEF32 (int16_t, int8_t)                                                      \
  DEF32 (uint8_t, uint64_t)                                                    \
  DEF32 (uint8_t, uint32_t)                                                    \
  DEF32 (uint8_t, uint16_t)                                                    \
  DEF32 (uint8_t, uint8_t)                                                     \
  DEF32 (uint8_t, int64_t)                                                     \
  DEF32 (uint8_t, int32_t)                                                     \
  DEF32 (uint8_t, int16_t)                                                     \
  DEF32 (uint8_t, int8_t)                                                      \
  DEF32 (int8_t, uint64_t)                                                     \
  DEF32 (int8_t, uint32_t)                                                     \
  DEF32 (int8_t, uint16_t)                                                     \
  DEF32 (int8_t, uint8_t)                                                      \
  DEF32 (int8_t, int64_t)                                                      \
  DEF32 (int8_t, int32_t)                                                      \
  DEF32 (int8_t, int16_t)                                                      \
  DEF32 (int8_t, int8_t)                                                       \
  DEFCTZ64 (uint64_t, uint64_t)                                                \
  DEFCTZ64 (uint64_t, uint32_t)                                                \
  DEFCTZ64 (uint64_t, uint16_t)                                                \
  DEFCTZ64 (uint64_t, uint8_t)                                                 \
  DEFCTZ64 (uint64_t, int64_t)                                                 \
  DEFCTZ64 (uint64_t, int32_t)                                                 \
  DEFCTZ64 (uint64_t, int16_t)                                                 \
  DEFCTZ64 (uint64_t, int8_t)                                                  \
  DEFCTZ64 (int64_t, uint64_t)                                                 \
  DEFCTZ64 (int64_t, uint32_t)                                                 \
  DEFCTZ64 (int64_t, uint16_t)                                                 \
  DEFCTZ64 (int64_t, uint8_t)                                                  \
  DEFCTZ64 (int64_t, int64_t)                                                  \
  DEFCTZ64 (int64_t, int32_t)                                                  \
  DEFCTZ64 (int64_t, int16_t)                                                  \
  DEFCTZ64 (int64_t, int8_t)                                                   \
  DEFCTZ64 (uint32_t, uint64_t)                                                \
  DEFCTZ64 (uint32_t, uint32_t)                                                \
  DEFCTZ64 (uint32_t, uint16_t)                                                \
  DEFCTZ64 (uint32_t, uint8_t)                                                 \
  DEFCTZ64 (uint32_t, int64_t)                                                 \
  DEFCTZ64 (uint32_t, int32_t)                                                 \
  DEFCTZ64 (uint32_t, int16_t)                                                 \
  DEFCTZ64 (uint32_t, int8_t)                                                  \
  DEFCTZ64 (int32_t, uint64_t)                                                 \
  DEFCTZ64 (int32_t, uint32_t)                                                 \
  DEFCTZ64 (int32_t, uint16_t)                                                 \
  DEFCTZ64 (int32_t, uint8_t)                                                  \
  DEFCTZ64 (int32_t, int64_t)                                                  \
  DEFCTZ64 (int32_t, int32_t)                                                  \
  DEFCTZ64 (int32_t, int16_t)                                                  \
  DEFCTZ64 (int32_t, int8_t)                                                   \
  DEFCTZ64 (uint16_t, uint64_t)                                                \
  DEFCTZ64 (uint16_t, uint32_t)                                                \
  DEFCTZ64 (uint16_t, uint16_t)                                                \
  DEFCTZ64 (uint16_t, uint8_t)                                                 \
  DEFCTZ64 (uint16_t, int64_t)                                                 \
  DEFCTZ64 (uint16_t, int32_t)                                                 \
  DEFCTZ64 (uint16_t, int16_t)                                                 \
  DEFCTZ64 (uint16_t, int8_t)                                                  \
  DEFCTZ64 (int16_t, uint64_t)                                                 \
  DEFCTZ64 (int16_t, uint32_t)                                                 \
  DEFCTZ64 (int16_t, uint16_t)                                                 \
  DEFCTZ64 (int16_t, uint8_t)                                                  \
  DEFCTZ64 (int16_t, int64_t)                                                  \
  DEFCTZ64 (int16_t, int32_t)                                                  \
  DEFCTZ64 (int16_t, int16_t)                                                  \
  DEFCTZ64 (int16_t, int8_t)                                                   \
  DEFCTZ64 (uint8_t, uint64_t)                                                 \
  DEFCTZ64 (uint8_t, uint32_t)                                                 \
  DEFCTZ64 (uint8_t, uint16_t)                                                 \
  DEFCTZ64 (uint8_t, uint8_t)                                                  \
  DEFCTZ64 (uint8_t, int64_t)                                                  \
  DEFCTZ64 (uint8_t, int32_t)                                                  \
  DEFCTZ64 (uint8_t, int16_t)                                                  \
  DEFCTZ64 (uint8_t, int8_t)                                                   \
  DEFCTZ64 (int8_t, uint64_t)                                                  \
  DEFCTZ64 (int8_t, uint32_t)                                                  \
  DEFCTZ64 (int8_t, uint16_t)                                                  \
  DEFCTZ64 (int8_t, uint8_t)                                                   \
  DEFCTZ64 (int8_t, int64_t)                                                   \
  DEFCTZ64 (int8_t, int32_t)                                                   \
  DEFCTZ64 (int8_t, int16_t)                                                   \
  DEFCTZ64 (int8_t, int8_t)                                                    \
  DEFCTZ32 (uint64_t, uint64_t)                                                \
  DEFCTZ32 (uint64_t, uint32_t)                                                \
  DEFCTZ32 (uint64_t, uint16_t)                                                \
  DEFCTZ32 (uint64_t, uint8_t)                                                 \
  DEFCTZ32 (uint64_t, int64_t)                                                 \
  DEFCTZ32 (uint64_t, int32_t)                                                 \
  DEFCTZ32 (uint64_t, int16_t)                                                 \
  DEFCTZ32 (uint64_t, int8_t)                                                  \
  DEFCTZ32 (int64_t, uint64_t)                                                 \
  DEFCTZ32 (int64_t, uint32_t)                                                 \
  DEFCTZ32 (int64_t, uint16_t)                                                 \
  DEFCTZ32 (int64_t, uint8_t)                                                  \
  DEFCTZ32 (int64_t, int64_t)                                                  \
  DEFCTZ32 (int64_t, int32_t)                                                  \
  DEFCTZ32 (int64_t, int16_t)                                                  \
  DEFCTZ32 (int64_t, int8_t)                                                   \
  DEFCTZ32 (uint32_t, uint64_t)                                                \
  DEFCTZ32 (uint32_t, uint32_t)                                                \
  DEFCTZ32 (uint32_t, uint16_t)                                                \
  DEFCTZ32 (uint32_t, uint8_t)                                                 \
  DEFCTZ32 (uint32_t, int64_t)                                                 \
  DEFCTZ32 (uint32_t, int32_t)                                                 \
  DEFCTZ32 (uint32_t, int16_t)                                                 \
  DEFCTZ32 (uint32_t, int8_t)                                                  \
  DEFCTZ32 (int32_t, uint64_t)                                                 \
  DEFCTZ32 (int32_t, uint32_t)                                                 \
  DEFCTZ32 (int32_t, uint16_t)                                                 \
  DEFCTZ32 (int32_t, uint8_t)                                                  \
  DEFCTZ32 (int32_t, int64_t)                                                  \
  DEFCTZ32 (int32_t, int32_t)                                                  \
  DEFCTZ32 (int32_t, int16_t)                                                  \
  DEFCTZ32 (int32_t, int8_t)                                                   \
  DEFCTZ32 (uint16_t, uint64_t)                                                \
  DEFCTZ32 (uint16_t, uint32_t)                                                \
  DEFCTZ32 (uint16_t, uint16_t)                                                \
  DEFCTZ32 (uint16_t, uint8_t)                                                 \
  DEFCTZ32 (uint16_t, int64_t)                                                 \
  DEFCTZ32 (uint16_t, int32_t)                                                 \
  DEFCTZ32 (uint16_t, int16_t)                                                 \
  DEFCTZ32 (uint16_t, int8_t)                                                  \
  DEFCTZ32 (int16_t, uint64_t)                                                 \
  DEFCTZ32 (int16_t, uint32_t)                                                 \
  DEFCTZ32 (int16_t, uint16_t)                                                 \
  DEFCTZ32 (int16_t, uint8_t)                                                  \
  DEFCTZ32 (int16_t, int64_t)                                                  \
  DEFCTZ32 (int16_t, int32_t)                                                  \
  DEFCTZ32 (int16_t, int16_t)                                                  \
  DEFCTZ32 (int16_t, int8_t)                                                   \
  DEFCTZ32 (uint8_t, uint64_t)                                                 \
  DEFCTZ32 (uint8_t, uint32_t)                                                 \
  DEFCTZ32 (uint8_t, uint16_t)                                                 \
  DEFCTZ32 (uint8_t, uint8_t)                                                  \
  DEFCTZ32 (uint8_t, int64_t)                                                  \
  DEFCTZ32 (uint8_t, int32_t)                                                  \
  DEFCTZ32 (uint8_t, int16_t)                                                  \
  DEFCTZ32 (uint8_t, int8_t)                                                   \
  DEFCTZ32 (int8_t, uint64_t)                                                  \
  DEFCTZ32 (int8_t, uint32_t)                                                  \
  DEFCTZ32 (int8_t, uint16_t)                                                  \
  DEFCTZ32 (int8_t, uint8_t)                                                   \
  DEFCTZ32 (int8_t, int64_t)                                                   \
  DEFCTZ32 (int8_t, int32_t)                                                   \
  DEFCTZ32 (int8_t, int16_t)                                                   \
  DEFCTZ32 (int8_t, int8_t)                                                    \
  DEFFFS64 (uint64_t, uint64_t)                                                \
  DEFFFS64 (uint64_t, uint32_t)                                                \
  DEFFFS64 (uint64_t, uint16_t)                                                \
  DEFFFS64 (uint64_t, uint8_t)                                                 \
  DEFFFS64 (uint64_t, int64_t)                                                 \
  DEFFFS64 (uint64_t, int32_t)                                                 \
  DEFFFS64 (uint64_t, int16_t)                                                 \
  DEFFFS64 (uint64_t, int8_t)                                                  \
  DEFFFS64 (int64_t, uint64_t)                                                 \
  DEFFFS64 (int64_t, uint32_t)                                                 \
  DEFFFS64 (int64_t, uint16_t)                                                 \
  DEFFFS64 (int64_t, uint8_t)                                                  \
  DEFFFS64 (int64_t, int64_t)                                                  \
  DEFFFS64 (int64_t, int32_t)                                                  \
  DEFFFS64 (int64_t, int16_t)                                                  \
  DEFFFS64 (int64_t, int8_t)                                                   \
  DEFFFS64 (uint32_t, uint64_t)                                                \
  DEFFFS64 (uint32_t, uint32_t)                                                \
  DEFFFS64 (uint32_t, uint16_t)                                                \
  DEFFFS64 (uint32_t, uint8_t)                                                 \
  DEFFFS64 (uint32_t, int64_t)                                                 \
  DEFFFS64 (uint32_t, int32_t)                                                 \
  DEFFFS64 (uint32_t, int16_t)                                                 \
  DEFFFS64 (uint32_t, int8_t)                                                  \
  DEFFFS64 (int32_t, uint64_t)                                                 \
  DEFFFS64 (int32_t, uint32_t)                                                 \
  DEFFFS64 (int32_t, uint16_t)                                                 \
  DEFFFS64 (int32_t, uint8_t)                                                  \
  DEFFFS64 (int32_t, int64_t)                                                  \
  DEFFFS64 (int32_t, int32_t)                                                  \
  DEFFFS64 (int32_t, int16_t)                                                  \
  DEFFFS64 (int32_t, int8_t)                                                   \
  DEFFFS64 (uint16_t, uint64_t)                                                \
  DEFFFS64 (uint16_t, uint32_t)                                                \
  DEFFFS64 (uint16_t, uint16_t)                                                \
  DEFFFS64 (uint16_t, uint8_t)                                                 \
  DEFFFS64 (uint16_t, int64_t)                                                 \
  DEFFFS64 (uint16_t, int32_t)                                                 \
  DEFFFS64 (uint16_t, int16_t)                                                 \
  DEFFFS64 (uint16_t, int8_t)                                                  \
  DEFFFS64 (int16_t, uint64_t)                                                 \
  DEFFFS64 (int16_t, uint32_t)                                                 \
  DEFFFS64 (int16_t, uint16_t)                                                 \
  DEFFFS64 (int16_t, uint8_t)                                                  \
  DEFFFS64 (int16_t, int64_t)                                                  \
  DEFFFS64 (int16_t, int32_t)                                                  \
  DEFFFS64 (int16_t, int16_t)                                                  \
  DEFFFS64 (int16_t, int8_t)                                                   \
  DEFFFS64 (uint8_t, uint64_t)                                                 \
  DEFFFS64 (uint8_t, uint32_t)                                                 \
  DEFFFS64 (uint8_t, uint16_t)                                                 \
  DEFFFS64 (uint8_t, uint8_t)                                                  \
  DEFFFS64 (uint8_t, int64_t)                                                  \
  DEFFFS64 (uint8_t, int32_t)                                                  \
  DEFFFS64 (uint8_t, int16_t)                                                  \
  DEFFFS64 (uint8_t, int8_t)                                                   \
  DEFFFS64 (int8_t, uint64_t)                                                  \
  DEFFFS64 (int8_t, uint32_t)                                                  \
  DEFFFS64 (int8_t, uint16_t)                                                  \
  DEFFFS64 (int8_t, uint8_t)                                                   \
  DEFFFS64 (int8_t, int64_t)                                                   \
  DEFFFS64 (int8_t, int32_t)                                                   \
  DEFFFS64 (int8_t, int16_t)                                                   \
  DEFFFS64 (int8_t, int8_t)                                                    \
  DEFFFS32 (uint64_t, uint64_t)                                                \
  DEFFFS32 (uint64_t, uint32_t)                                                \
  DEFFFS32 (uint64_t, uint16_t)                                                \
  DEFFFS32 (uint64_t, uint8_t)                                                 \
  DEFFFS32 (uint64_t, int64_t)                                                 \
  DEFFFS32 (uint64_t, int32_t)                                                 \
  DEFFFS32 (uint64_t, int16_t)                                                 \
  DEFFFS32 (uint64_t, int8_t)                                                  \
  DEFFFS32 (int64_t, uint64_t)                                                 \
  DEFFFS32 (int64_t, uint32_t)                                                 \
  DEFFFS32 (int64_t, uint16_t)                                                 \
  DEFFFS32 (int64_t, uint8_t)                                                  \
  DEFFFS32 (int64_t, int64_t)                                                  \
  DEFFFS32 (int64_t, int32_t)                                                  \
  DEFFFS32 (int64_t, int16_t)                                                  \
  DEFFFS32 (int64_t, int8_t)                                                   \
  DEFFFS32 (uint32_t, uint64_t)                                                \
  DEFFFS32 (uint32_t, uint32_t)                                                \
  DEFFFS32 (uint32_t, uint16_t)                                                \
  DEFFFS32 (uint32_t, uint8_t)                                                 \
  DEFFFS32 (uint32_t, int64_t)                                                 \
  DEFFFS32 (uint32_t, int32_t)                                                 \
  DEFFFS32 (uint32_t, int16_t)                                                 \
  DEFFFS32 (uint32_t, int8_t)                                                  \
  DEFFFS32 (int32_t, uint64_t)                                                 \
  DEFFFS32 (int32_t, uint32_t)                                                 \
  DEFFFS32 (int32_t, uint16_t)                                                 \
  DEFFFS32 (int32_t, uint8_t)                                                  \
  DEFFFS32 (int32_t, int64_t)                                                  \
  DEFFFS32 (int32_t, int32_t)                                                  \
  DEFFFS32 (int32_t, int16_t)                                                  \
  DEFFFS32 (int32_t, int8_t)                                                   \
  DEFFFS32 (uint16_t, uint64_t)                                                \
  DEFFFS32 (uint16_t, uint32_t)                                                \
  DEFFFS32 (uint16_t, uint16_t)                                                \
  DEFFFS32 (uint16_t, uint8_t)                                                 \
  DEFFFS32 (uint16_t, int64_t)                                                 \
  DEFFFS32 (uint16_t, int32_t)                                                 \
  DEFFFS32 (uint16_t, int16_t)                                                 \
  DEFFFS32 (uint16_t, int8_t)                                                  \
  DEFFFS32 (int16_t, uint64_t)                                                 \
  DEFFFS32 (int16_t, uint32_t)                                                 \
  DEFFFS32 (int16_t, uint16_t)                                                 \
  DEFFFS32 (int16_t, uint8_t)                                                  \
  DEFFFS32 (int16_t, int64_t)                                                  \
  DEFFFS32 (int16_t, int32_t)                                                  \
  DEFFFS32 (int16_t, int16_t)                                                  \
  DEFFFS32 (int16_t, int8_t)                                                   \
  DEFFFS32 (uint8_t, uint64_t)                                                 \
  DEFFFS32 (uint8_t, uint32_t)                                                 \
  DEFFFS32 (uint8_t, uint16_t)                                                 \
  DEFFFS32 (uint8_t, uint8_t)                                                  \
  DEFFFS32 (uint8_t, int64_t)                                                  \
  DEFFFS32 (uint8_t, int32_t)                                                  \
  DEFFFS32 (uint8_t, int16_t)                                                  \
  DEFFFS32 (uint8_t, int8_t)                                                   \
  DEFFFS32 (int8_t, uint64_t)                                                  \
  DEFFFS32 (int8_t, uint32_t)                                                  \
  DEFFFS32 (int8_t, uint16_t)                                                  \
  DEFFFS32 (int8_t, uint8_t)                                                   \
  DEFFFS32 (int8_t, int64_t)                                                   \
  DEFFFS32 (int8_t, int32_t)                                                   \
  DEFFFS32 (int8_t, int16_t)                                                   \
  DEFFFS32 (int8_t, int8_t)

DEF_ALL ()

#define SZ 512

#define TEST64(TYPEDST, TYPESRC)                                               \
  void __attribute__ ((optimize ("0"))) test64_##TYPEDST##TYPESRC ()           \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * 1234567890;                                              \
	dst[i] = 0;                                                            \
      }                                                                        \
    popcount64_##TYPEDST##TYPESRC (dst, src, SZ);                              \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	assert (dst[i] == __builtin_popcountll (src[i]));                      \
      }                                                                        \
  }

#define TEST64N(TYPEDST, TYPESRC)                                              \
  void __attribute__ ((optimize ("0"))) test64n_##TYPEDST##TYPESRC ()          \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * -1234567890;                                             \
	dst[i] = 0;                                                            \
      }                                                                        \
    popcount64_##TYPEDST##TYPESRC (dst, src, SZ);                              \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	assert (dst[i] == __builtin_popcountll (src[i]));                      \
      }                                                                        \
  }

#define TEST32(TYPEDST, TYPESRC)                                               \
  void __attribute__ ((optimize ("0"))) test32_##TYPEDST##TYPESRC ()           \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * 1234567;                                                 \
	dst[i] = 0;                                                            \
      }                                                                        \
    popcount32_##TYPEDST##TYPESRC (dst, src, SZ);                              \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	assert (dst[i] == __builtin_popcount (src[i]));                        \
      }                                                                        \
  }

#define TEST32N(TYPEDST, TYPESRC)                                              \
  void __attribute__ ((optimize ("0"))) test32n_##TYPEDST##TYPESRC ()          \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * -1234567;                                                \
	dst[i] = 0;                                                            \
      }                                                                        \
    popcount32_##TYPEDST##TYPESRC (dst, src, SZ);                              \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	assert (dst[i] == __builtin_popcount (src[i]));                        \
      }                                                                        \
  }

#define TESTCTZ64(TYPEDST, TYPESRC)                                            \
  void __attribute__ ((optimize ("0"))) testctz64_##TYPEDST##TYPESRC ()        \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * 1234567890;                                              \
	dst[i] = 0;                                                            \
      }                                                                        \
    ctz64_##TYPEDST##TYPESRC (dst, src, SZ);                                   \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	if (src[i] != 0)                                                       \
	  assert (dst[i] == __builtin_ctzll (src[i]));                         \
      }                                                                        \
  }

#define TESTCTZ64N(TYPEDST, TYPESRC)                                           \
  void __attribute__ ((optimize ("0"))) testctz64n_##TYPEDST##TYPESRC ()       \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * -1234567890;                                             \
	dst[i] = 0;                                                            \
      }                                                                        \
    ctz64_##TYPEDST##TYPESRC (dst, src, SZ);                                   \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	if (src[i] != 0)                                                       \
	  assert (dst[i] == __builtin_ctzll (src[i]));                         \
      }                                                                        \
  }

#define TESTCTZ32(TYPEDST, TYPESRC)                                            \
  void __attribute__ ((optimize ("0"))) testctz32_##TYPEDST##TYPESRC ()        \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * 1234567;                                                 \
	dst[i] = 0;                                                            \
      }                                                                        \
    ctz32_##TYPEDST##TYPESRC (dst, src, SZ);                                   \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	if (src[i] != 0)                                                       \
	  assert (dst[i] == __builtin_ctz (src[i]));                           \
      }                                                                        \
  }

#define TESTCTZ32N(TYPEDST, TYPESRC)                                           \
  void __attribute__ ((optimize ("0"))) testctz32n_##TYPEDST##TYPESRC ()       \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * -1234567;                                                \
	dst[i] = 0;                                                            \
      }                                                                        \
    ctz32_##TYPEDST##TYPESRC (dst, src, SZ);                                   \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	if (src[i] != 0)                                                       \
	  assert (dst[i] == __builtin_ctz (src[i]));                           \
      }                                                                        \
  }

#define TESTFFS64(TYPEDST, TYPESRC)                                            \
  void __attribute__ ((optimize ("0"))) testffs64_##TYPEDST##TYPESRC ()        \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * 1234567890;                                              \
	dst[i] = 0;                                                            \
      }                                                                        \
    ffs64_##TYPEDST##TYPESRC (dst, src, SZ);                                   \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	assert (dst[i] == __builtin_ffsll (src[i]));                           \
      }                                                                        \
  }

#define TESTFFS64N(TYPEDST, TYPESRC)                                           \
  void __attribute__ ((optimize ("0"))) testffs64n_##TYPEDST##TYPESRC ()       \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * -1234567890;                                             \
	dst[i] = 0;                                                            \
      }                                                                        \
    ffs64_##TYPEDST##TYPESRC (dst, src, SZ);                                   \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	assert (dst[i] == __builtin_ffsll (src[i]));                           \
      }                                                                        \
  }

#define TESTFFS32(TYPEDST, TYPESRC)                                            \
  void __attribute__ ((optimize ("0"))) testffs32_##TYPEDST##TYPESRC ()        \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * 1234567;                                                 \
	dst[i] = 0;                                                            \
      }                                                                        \
    ffs32_##TYPEDST##TYPESRC (dst, src, SZ);                                   \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	assert (dst[i] == __builtin_ffs (src[i]));                             \
      }                                                                        \
  }

#define TESTFFS32N(TYPEDST, TYPESRC)                                           \
  void __attribute__ ((optimize ("0"))) testffs32n_##TYPEDST##TYPESRC ()       \
  {                                                                            \
    TYPESRC src[SZ];                                                           \
    TYPEDST dst[SZ];                                                           \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	int ia = i + 1;                                                        \
	src[i] = ia * -1234567;                                                \
	dst[i] = 0;                                                            \
      }                                                                        \
    ffs32_##TYPEDST##TYPESRC (dst, src, SZ);                                   \
    for (int i = 0; i < SZ; i++)                                               \
      {                                                                        \
	assert (dst[i] == __builtin_ffs (src[i]));                             \
      }                                                                        \
  }

#define TEST_ALL()                                                             \
  TEST64 (uint64_t, uint64_t)                                                  \
  TEST64 (uint64_t, uint32_t)                                                  \
  TEST64 (uint64_t, uint16_t)                                                  \
  TEST64 (uint64_t, uint8_t)                                                   \
  TEST64 (uint64_t, int64_t)                                                   \
  TEST64 (uint64_t, int32_t)                                                   \
  TEST64 (uint64_t, int16_t)                                                   \
  TEST64 (uint64_t, int8_t)                                                    \
  TEST64N (int64_t, uint64_t)                                                  \
  TEST64N (int64_t, uint32_t)                                                  \
  TEST64N (int64_t, uint16_t)                                                  \
  TEST64N (int64_t, uint8_t)                                                   \
  TEST64N (int64_t, int64_t)                                                   \
  TEST64N (int64_t, int32_t)                                                   \
  TEST64N (int64_t, int16_t)                                                   \
  TEST64N (int64_t, int8_t)                                                    \
  TEST64 (uint32_t, uint64_t)                                                  \
  TEST64 (uint32_t, uint32_t)                                                  \
  TEST64 (uint32_t, uint16_t)                                                  \
  TEST64 (uint32_t, uint8_t)                                                   \
  TEST64 (uint32_t, int64_t)                                                   \
  TEST64 (uint32_t, int32_t)                                                   \
  TEST64 (uint32_t, int16_t)                                                   \
  TEST64 (uint32_t, int8_t)                                                    \
  TEST64N (int32_t, uint64_t)                                                  \
  TEST64N (int32_t, uint32_t)                                                  \
  TEST64N (int32_t, uint16_t)                                                  \
  TEST64N (int32_t, uint8_t)                                                   \
  TEST64N (int32_t, int64_t)                                                   \
  TEST64N (int32_t, int32_t)                                                   \
  TEST64N (int32_t, int16_t)                                                   \
  TEST64N (int32_t, int8_t)                                                    \
  TEST64 (uint16_t, uint64_t)                                                  \
  TEST64 (uint16_t, uint32_t)                                                  \
  TEST64 (uint16_t, uint16_t)                                                  \
  TEST64 (uint16_t, uint8_t)                                                   \
  TEST64 (uint16_t, int64_t)                                                   \
  TEST64 (uint16_t, int32_t)                                                   \
  TEST64 (uint16_t, int16_t)                                                   \
  TEST64 (uint16_t, int8_t)                                                    \
  TEST64N (int16_t, uint64_t)                                                   \
  TEST64N (int16_t, uint32_t)                                                   \
  TEST64N (int16_t, uint16_t)                                                   \
  TEST64N (int16_t, uint8_t)                                                    \
  TEST64N (int16_t, int64_t)                                                    \
  TEST64N (int16_t, int32_t)                                                    \
  TEST64N (int16_t, int16_t)                                                    \
  TEST64N (int16_t, int8_t)                                                     \
  TEST64 (uint8_t, uint64_t)                                                   \
  TEST64 (uint8_t, uint32_t)                                                   \
  TEST64 (uint8_t, uint16_t)                                                   \
  TEST64 (uint8_t, uint8_t)                                                    \
  TEST64 (uint8_t, int64_t)                                                    \
  TEST64 (uint8_t, int32_t)                                                    \
  TEST64 (uint8_t, int16_t)                                                    \
  TEST64 (uint8_t, int8_t)                                                     \
  TEST64N (int8_t, uint64_t)                                                    \
  TEST64N (int8_t, uint32_t)                                                    \
  TEST64N (int8_t, uint16_t)                                                    \
  TEST64N (int8_t, uint8_t)                                                     \
  TEST64N (int8_t, int64_t)                                                     \
  TEST64N (int8_t, int32_t)                                                     \
  TEST64N (int8_t, int16_t)                                                     \
  TEST64N (int8_t, int8_t)                                                      \
  TEST32 (uint64_t, uint64_t)                                                  \
  TEST32 (uint64_t, uint32_t)                                                  \
  TEST32 (uint64_t, uint16_t)                                                  \
  TEST32 (uint64_t, uint8_t)                                                   \
  TEST32 (uint64_t, int64_t)                                                   \
  TEST32 (uint64_t, int32_t)                                                   \
  TEST32 (uint64_t, int16_t)                                                   \
  TEST32 (uint64_t, int8_t)                                                    \
  TEST32N (int64_t, uint64_t)                                                  \
  TEST32N (int64_t, uint32_t)                                                  \
  TEST32N (int64_t, uint16_t)                                                  \
  TEST32N (int64_t, uint8_t)                                                   \
  TEST32N (int64_t, int64_t)                                                   \
  TEST32N (int64_t, int32_t)                                                   \
  TEST32N (int64_t, int16_t)                                                   \
  TEST32N (int64_t, int8_t)                                                    \
  TEST32 (uint32_t, uint64_t)                                                  \
  TEST32 (uint32_t, uint32_t)                                                  \
  TEST32 (uint32_t, uint16_t)                                                  \
  TEST32 (uint32_t, uint8_t)                                                   \
  TEST32 (uint32_t, int64_t)                                                   \
  TEST32 (uint32_t, int32_t)                                                   \
  TEST32 (uint32_t, int16_t)                                                   \
  TEST32 (uint32_t, int8_t)                                                    \
  TEST32N (int32_t, uint64_t)                                                  \
  TEST32N (int32_t, uint32_t)                                                  \
  TEST32N (int32_t, uint16_t)                                                  \
  TEST32N (int32_t, uint8_t)                                                   \
  TEST32N (int32_t, int64_t)                                                   \
  TEST32N (int32_t, int32_t)                                                   \
  TEST32N (int32_t, int16_t)                                                   \
  TEST32N (int32_t, int8_t)                                                    \
  TEST32 (uint16_t, uint64_t)                                                  \
  TEST32 (uint16_t, uint32_t)                                                  \
  TEST32 (uint16_t, uint16_t)                                                  \
  TEST32 (uint16_t, uint8_t)                                                   \
  TEST32 (uint16_t, int64_t)                                                   \
  TEST32 (uint16_t, int32_t)                                                   \
  TEST32 (uint16_t, int16_t)                                                   \
  TEST32 (uint16_t, int8_t)                                                    \
  TEST32N (int16_t, uint64_t)                                                  \
  TEST32N (int16_t, uint32_t)                                                  \
  TEST32N (int16_t, uint16_t)                                                  \
  TEST32N (int16_t, uint8_t)                                                   \
  TEST32N (int16_t, int64_t)                                                   \
  TEST32N (int16_t, int32_t)                                                   \
  TEST32N (int16_t, int16_t)                                                   \
  TEST32N (int16_t, int8_t)                                                    \
  TEST32 (uint8_t, uint64_t)                                                   \
  TEST32 (uint8_t, uint32_t)                                                   \
  TEST32 (uint8_t, uint16_t)                                                   \
  TEST32 (uint8_t, uint8_t)                                                    \
  TEST32 (uint8_t, int64_t)                                                    \
  TEST32 (uint8_t, int32_t)                                                    \
  TEST32 (uint8_t, int16_t)                                                    \
  TEST32 (uint8_t, int8_t)                                                     \
  TEST32N (int8_t, uint64_t)                                                   \
  TEST32N (int8_t, uint32_t)                                                   \
  TEST32N (int8_t, uint16_t)                                                   \
  TEST32N (int8_t, uint8_t)                                                    \
  TEST32N (int8_t, int64_t)                                                    \
  TEST32N (int8_t, int32_t)                                                    \
  TEST32N (int8_t, int16_t)                                                    \
  TEST32N (int8_t, int8_t)                                                     \
  TESTCTZ64 (uint64_t, uint64_t)                                               \
  TESTCTZ64 (uint64_t, uint32_t)                                               \
  TESTCTZ64 (uint64_t, uint16_t)                                               \
  TESTCTZ64 (uint64_t, uint8_t)                                                \
  TESTCTZ64 (uint64_t, int64_t)                                                \
  TESTCTZ64 (uint64_t, int32_t)                                                \
  TESTCTZ64 (uint64_t, int16_t)                                                \
  TESTCTZ64 (uint64_t, int8_t)                                                 \
  TESTCTZ64N (int64_t, uint64_t)                                               \
  TESTCTZ64N (int64_t, uint32_t)                                               \
  TESTCTZ64N (int64_t, uint16_t)                                               \
  TESTCTZ64N (int64_t, uint8_t)                                                \
  TESTCTZ64N (int64_t, int64_t)                                                \
  TESTCTZ64N (int64_t, int32_t)                                                \
  TESTCTZ64N (int64_t, int16_t)                                                \
  TESTCTZ64N (int64_t, int8_t)                                                 \
  TESTCTZ64 (uint32_t, uint64_t)                                               \
  TESTCTZ64 (uint32_t, uint32_t)                                               \
  TESTCTZ64 (uint32_t, uint16_t)                                               \
  TESTCTZ64 (uint32_t, uint8_t)                                                \
  TESTCTZ64 (uint32_t, int64_t)                                                \
  TESTCTZ64 (uint32_t, int32_t)                                                \
  TESTCTZ64 (uint32_t, int16_t)                                                \
  TESTCTZ64 (uint32_t, int8_t)                                                 \
  TESTCTZ64N (int32_t, uint64_t)                                               \
  TESTCTZ64N (int32_t, uint32_t)                                               \
  TESTCTZ64N (int32_t, uint16_t)                                               \
  TESTCTZ64N (int32_t, uint8_t)                                                \
  TESTCTZ64N (int32_t, int64_t)                                                \
  TESTCTZ64N (int32_t, int32_t)                                                \
  TESTCTZ64N (int32_t, int16_t)                                                \
  TESTCTZ64N (int32_t, int8_t)                                                 \
  TESTCTZ64 (uint16_t, uint64_t)                                               \
  TESTCTZ64 (uint16_t, uint32_t)                                               \
  TESTCTZ64 (uint16_t, uint16_t)                                               \
  TESTCTZ64 (uint16_t, uint8_t)                                                \
  TESTCTZ64 (uint16_t, int64_t)                                                \
  TESTCTZ64 (uint16_t, int32_t)                                                \
  TESTCTZ64 (uint16_t, int16_t)                                                \
  TESTCTZ64 (uint16_t, int8_t)                                                 \
  TESTCTZ64N (int16_t, uint64_t)                                               \
  TESTCTZ64N (int16_t, uint32_t)                                               \
  TESTCTZ64N (int16_t, uint16_t)                                               \
  TESTCTZ64N (int16_t, uint8_t)                                                \
  TESTCTZ64N (int16_t, int64_t)                                                \
  TESTCTZ64N (int16_t, int32_t)                                                \
  TESTCTZ64N (int16_t, int16_t)                                                \
  TESTCTZ64N (int16_t, int8_t)                                                 \
  TESTCTZ64 (uint8_t, uint64_t)                                                \
  TESTCTZ64 (uint8_t, uint32_t)                                                \
  TESTCTZ64 (uint8_t, uint16_t)                                                \
  TESTCTZ64 (uint8_t, uint8_t)                                                 \
  TESTCTZ64 (uint8_t, int64_t)                                                 \
  TESTCTZ64 (uint8_t, int32_t)                                                 \
  TESTCTZ64 (uint8_t, int16_t)                                                 \
  TESTCTZ64 (uint8_t, int8_t)                                                  \
  TESTCTZ64N (int8_t, uint64_t)                                                \
  TESTCTZ64N (int8_t, uint32_t)                                                \
  TESTCTZ64N (int8_t, uint16_t)                                                \
  TESTCTZ64N (int8_t, uint8_t)                                                 \
  TESTCTZ64N (int8_t, int64_t)                                                 \
  TESTCTZ64N (int8_t, int32_t)                                                 \
  TESTCTZ64N (int8_t, int16_t)                                                 \
  TESTCTZ64N (int8_t, int8_t)                                                  \
  TESTCTZ32 (uint64_t, uint64_t)                                               \
  TESTCTZ32 (uint64_t, uint32_t)                                               \
  TESTCTZ32 (uint64_t, uint16_t)                                               \
  TESTCTZ32 (uint64_t, uint8_t)                                                \
  TESTCTZ32 (uint64_t, int64_t)                                                \
  TESTCTZ32 (uint64_t, int32_t)                                                \
  TESTCTZ32 (uint64_t, int16_t)                                                \
  TESTCTZ32 (uint64_t, int8_t)                                                 \
  TESTCTZ32N (int64_t, uint64_t)                                               \
  TESTCTZ32N (int64_t, uint32_t)                                               \
  TESTCTZ32N (int64_t, uint16_t)                                               \
  TESTCTZ32N (int64_t, uint8_t)                                                \
  TESTCTZ32N (int64_t, int64_t)                                                \
  TESTCTZ32N (int64_t, int32_t)                                                \
  TESTCTZ32N (int64_t, int16_t)                                                \
  TESTCTZ32N (int64_t, int8_t)                                                 \
  TESTCTZ32 (uint32_t, uint64_t)                                               \
  TESTCTZ32 (uint32_t, uint32_t)                                               \
  TESTCTZ32 (uint32_t, uint16_t)                                               \
  TESTCTZ32 (uint32_t, uint8_t)                                                \
  TESTCTZ32 (uint32_t, int64_t)                                                \
  TESTCTZ32 (uint32_t, int32_t)                                                \
  TESTCTZ32 (uint32_t, int16_t)                                                \
  TESTCTZ32 (uint32_t, int8_t)                                                 \
  TESTCTZ32N (int32_t, uint64_t)                                               \
  TESTCTZ32N (int32_t, uint32_t)                                               \
  TESTCTZ32N (int32_t, uint16_t)                                               \
  TESTCTZ32N (int32_t, uint8_t)                                                \
  TESTCTZ32N (int32_t, int64_t)                                                \
  TESTCTZ32N (int32_t, int32_t)                                                \
  TESTCTZ32N (int32_t, int16_t)                                                \
  TESTCTZ32N (int32_t, int8_t)                                                 \
  TESTCTZ32 (uint16_t, uint64_t)                                               \
  TESTCTZ32 (uint16_t, uint32_t)                                               \
  TESTCTZ32 (uint16_t, uint16_t)                                               \
  TESTCTZ32 (uint16_t, uint8_t)                                                \
  TESTCTZ32 (uint16_t, int64_t)                                                \
  TESTCTZ32 (uint16_t, int32_t)                                                \
  TESTCTZ32 (uint16_t, int16_t)                                                \
  TESTCTZ32 (uint16_t, int8_t)                                                 \
  TESTCTZ32N (int16_t, uint64_t)                                               \
  TESTCTZ32N (int16_t, uint32_t)                                               \
  TESTCTZ32N (int16_t, uint16_t)                                               \
  TESTCTZ32N (int16_t, uint8_t)                                                \
  TESTCTZ32N (int16_t, int64_t)                                                \
  TESTCTZ32N (int16_t, int32_t)                                                \
  TESTCTZ32N (int16_t, int16_t)                                                \
  TESTCTZ32N (int16_t, int8_t)                                                 \
  TESTCTZ32 (uint8_t, uint64_t)                                                \
  TESTCTZ32 (uint8_t, uint32_t)                                                \
  TESTCTZ32 (uint8_t, uint16_t)                                                \
  TESTCTZ32 (uint8_t, uint8_t)                                                 \
  TESTCTZ32 (uint8_t, int64_t)                                                 \
  TESTCTZ32 (uint8_t, int32_t)                                                 \
  TESTCTZ32 (uint8_t, int16_t)                                                 \
  TESTCTZ32 (uint8_t, int8_t)                                                  \
  TESTCTZ32N (int8_t, uint64_t)                                                \
  TESTCTZ32N (int8_t, uint32_t)                                                \
  TESTCTZ32N (int8_t, uint16_t)                                                \
  TESTCTZ32N (int8_t, uint8_t)                                                 \
  TESTCTZ32N (int8_t, int64_t)                                                 \
  TESTCTZ32N (int8_t, int32_t)                                                 \
  TESTCTZ32N (int8_t, int16_t)                                                 \
  TESTCTZ32N (int8_t, int8_t)                                                  \
  TESTFFS64 (uint64_t, uint64_t)                                               \
  TESTFFS64 (uint64_t, uint32_t)                                               \
  TESTFFS64 (uint64_t, uint16_t)                                               \
  TESTFFS64 (uint64_t, uint8_t)                                                \
  TESTFFS64 (uint64_t, int64_t)                                                \
  TESTFFS64 (uint64_t, int32_t)                                                \
  TESTFFS64 (uint64_t, int16_t)                                                \
  TESTFFS64 (uint64_t, int8_t)                                                 \
  TESTFFS64N (int64_t, uint64_t)                                               \
  TESTFFS64N (int64_t, uint32_t)                                               \
  TESTFFS64N (int64_t, uint16_t)                                               \
  TESTFFS64N (int64_t, uint8_t)                                                \
  TESTFFS64N (int64_t, int64_t)                                                \
  TESTFFS64N (int64_t, int32_t)                                                \
  TESTFFS64N (int64_t, int16_t)                                                \
  TESTFFS64N (int64_t, int8_t)                                                 \
  TESTFFS64 (uint32_t, uint64_t)                                               \
  TESTFFS64 (uint32_t, uint32_t)                                               \
  TESTFFS64 (uint32_t, uint16_t)                                               \
  TESTFFS64 (uint32_t, uint8_t)                                                \
  TESTFFS64 (uint32_t, int64_t)                                                \
  TESTFFS64 (uint32_t, int32_t)                                                \
  TESTFFS64 (uint32_t, int16_t)                                                \
  TESTFFS64 (uint32_t, int8_t)                                                 \
  TESTFFS64N (int32_t, uint64_t)                                               \
  TESTFFS64N (int32_t, uint32_t)                                               \
  TESTFFS64N (int32_t, uint16_t)                                               \
  TESTFFS64N (int32_t, uint8_t)                                                \
  TESTFFS64N (int32_t, int64_t)                                                \
  TESTFFS64N (int32_t, int32_t)                                                \
  TESTFFS64N (int32_t, int16_t)                                                \
  TESTFFS64N (int32_t, int8_t)                                                 \
  TESTFFS64 (uint16_t, uint64_t)                                               \
  TESTFFS64 (uint16_t, uint32_t)                                               \
  TESTFFS64 (uint16_t, uint16_t)                                               \
  TESTFFS64 (uint16_t, uint8_t)                                                \
  TESTFFS64 (uint16_t, int64_t)                                                \
  TESTFFS64 (uint16_t, int32_t)                                                \
  TESTFFS64 (uint16_t, int16_t)                                                \
  TESTFFS64 (uint16_t, int8_t)                                                 \
  TESTFFS64N (int16_t, uint64_t)                                               \
  TESTFFS64N (int16_t, uint32_t)                                               \
  TESTFFS64N (int16_t, uint16_t)                                               \
  TESTFFS64N (int16_t, uint8_t)                                                \
  TESTFFS64N (int16_t, int64_t)                                                \
  TESTFFS64N (int16_t, int32_t)                                                \
  TESTFFS64N (int16_t, int16_t)                                                \
  TESTFFS64N (int16_t, int8_t)                                                 \
  TESTFFS64 (uint8_t, uint64_t)                                                \
  TESTFFS64 (uint8_t, uint32_t)                                                \
  TESTFFS64 (uint8_t, uint16_t)                                                \
  TESTFFS64 (uint8_t, uint8_t)                                                 \
  TESTFFS64 (uint8_t, int64_t)                                                 \
  TESTFFS64 (uint8_t, int32_t)                                                 \
  TESTFFS64 (uint8_t, int16_t)                                                 \
  TESTFFS64 (uint8_t, int8_t)                                                  \
  TESTFFS64N (int8_t, uint64_t)                                                \
  TESTFFS64N (int8_t, uint32_t)                                                \
  TESTFFS64N (int8_t, uint16_t)                                                \
  TESTFFS64N (int8_t, uint8_t)                                                 \
  TESTFFS64N (int8_t, int64_t)                                                 \
  TESTFFS64N (int8_t, int32_t)                                                 \
  TESTFFS64N (int8_t, int16_t)                                                 \
  TESTFFS64N (int8_t, int8_t)                                                  \
  TESTFFS32 (uint64_t, uint64_t)                                               \
  TESTFFS32 (uint64_t, uint32_t)                                               \
  TESTFFS32 (uint64_t, uint16_t)                                               \
  TESTFFS32 (uint64_t, uint8_t)                                                \
  TESTFFS32 (uint64_t, int64_t)                                                \
  TESTFFS32 (uint64_t, int32_t)                                                \
  TESTFFS32 (uint64_t, int16_t)                                                \
  TESTFFS32 (uint64_t, int8_t)                                                 \
  TESTFFS32N (int64_t, uint64_t)                                               \
  TESTFFS32N (int64_t, uint32_t)                                               \
  TESTFFS32N (int64_t, uint16_t)                                               \
  TESTFFS32N (int64_t, uint8_t)                                                \
  TESTFFS32N (int64_t, int64_t)                                                \
  TESTFFS32N (int64_t, int32_t)                                                \
  TESTFFS32N (int64_t, int16_t)                                                \
  TESTFFS32N (int64_t, int8_t)                                                 \
  TESTFFS32 (uint32_t, uint64_t)                                               \
  TESTFFS32 (uint32_t, uint32_t)                                               \
  TESTFFS32 (uint32_t, uint16_t)                                               \
  TESTFFS32 (uint32_t, uint8_t)                                                \
  TESTFFS32 (uint32_t, int64_t)                                                \
  TESTFFS32 (uint32_t, int32_t)                                                \
  TESTFFS32 (uint32_t, int16_t)                                                \
  TESTFFS32 (uint32_t, int8_t)                                                 \
  TESTFFS32N (int32_t, uint64_t)                                               \
  TESTFFS32N (int32_t, uint32_t)                                               \
  TESTFFS32N (int32_t, uint16_t)                                               \
  TESTFFS32N (int32_t, uint8_t)                                                \
  TESTFFS32N (int32_t, int64_t)                                                \
  TESTFFS32N (int32_t, int32_t)                                                \
  TESTFFS32N (int32_t, int16_t)                                                \
  TESTFFS32N (int32_t, int8_t)                                                 \
  TESTFFS32 (uint16_t, uint64_t)                                               \
  TESTFFS32 (uint16_t, uint32_t)                                               \
  TESTFFS32 (uint16_t, uint16_t)                                               \
  TESTFFS32 (uint16_t, uint8_t)                                                \
  TESTFFS32 (uint16_t, int64_t)                                                \
  TESTFFS32 (uint16_t, int32_t)                                                \
  TESTFFS32 (uint16_t, int16_t)                                                \
  TESTFFS32 (uint16_t, int8_t)                                                 \
  TESTFFS32N (int16_t, uint64_t)                                               \
  TESTFFS32N (int16_t, uint32_t)                                               \
  TESTFFS32N (int16_t, uint16_t)                                               \
  TESTFFS32N (int16_t, uint8_t)                                                \
  TESTFFS32N (int16_t, int64_t)                                                \
  TESTFFS32N (int16_t, int32_t)                                                \
  TESTFFS32N (int16_t, int16_t)                                                \
  TESTFFS32N (int16_t, int8_t)                                                 \
  TESTFFS32 (uint8_t, uint64_t)                                                \
  TESTFFS32 (uint8_t, uint32_t)                                                \
  TESTFFS32 (uint8_t, uint16_t)                                                \
  TESTFFS32 (uint8_t, uint8_t)                                                 \
  TESTFFS32 (uint8_t, int64_t)                                                 \
  TESTFFS32 (uint8_t, int32_t)                                                 \
  TESTFFS32 (uint8_t, int16_t)                                                 \
  TESTFFS32 (uint8_t, int8_t)                                                  \
  TESTFFS32N (int8_t, uint64_t)                                                \
  TESTFFS32N (int8_t, uint32_t)                                                \
  TESTFFS32N (int8_t, uint16_t)                                                \
  TESTFFS32N (int8_t, uint8_t)                                                 \
  TESTFFS32N (int8_t, int64_t)                                                 \
  TESTFFS32N (int8_t, int32_t)                                                 \
  TESTFFS32N (int8_t, int16_t)                                                 \
  TESTFFS32N (int8_t, int8_t)

TEST_ALL ()

#define RUN64(TYPEDST, TYPESRC) test64_##TYPEDST##TYPESRC ();
#define RUN64N(TYPEDST, TYPESRC) test64n_##TYPEDST##TYPESRC ();
#define RUN32(TYPEDST, TYPESRC) test32_##TYPEDST##TYPESRC ();
#define RUN32N(TYPEDST, TYPESRC) test32n_##TYPEDST##TYPESRC ();
#define RUNCTZ64(TYPEDST, TYPESRC) testctz64_##TYPEDST##TYPESRC ();
#define RUNCTZ64N(TYPEDST, TYPESRC) testctz64n_##TYPEDST##TYPESRC ();
#define RUNCTZ32(TYPEDST, TYPESRC) testctz32_##TYPEDST##TYPESRC ();
#define RUNCTZ32N(TYPEDST, TYPESRC) testctz32n_##TYPEDST##TYPESRC ();
#define RUNFFS64(TYPEDST, TYPESRC) testffs64_##TYPEDST##TYPESRC ();
#define RUNFFS64N(TYPEDST, TYPESRC) testffs64n_##TYPEDST##TYPESRC ();
#define RUNFFS32(TYPEDST, TYPESRC) testffs32_##TYPEDST##TYPESRC ();
#define RUNFFS32N(TYPEDST, TYPESRC) testffs32n_##TYPEDST##TYPESRC ();

#define RUN_ALL()                                                              \
  RUN64 (uint64_t, uint64_t)                                                   \
  RUN64 (uint64_t, uint32_t)                                                   \
  RUN64 (uint64_t, uint16_t)                                                   \
  RUN64 (uint64_t, uint8_t)                                                    \
  RUN64 (uint64_t, int64_t)                                                    \
  RUN64 (uint64_t, int32_t)                                                    \
  RUN64 (uint64_t, int16_t)                                                    \
  RUN64 (uint64_t, int8_t)                                                     \
  RUN64N (int64_t, uint64_t)                                                    \
  RUN64N (int64_t, uint32_t)                                                    \
  RUN64N (int64_t, uint16_t)                                                    \
  RUN64N (int64_t, uint8_t)                                                     \
  RUN64N (int64_t, int64_t)                                                     \
  RUN64N (int64_t, int32_t)                                                     \
  RUN64N (int64_t, int16_t)                                                     \
  RUN64N (int64_t, int8_t)                                                      \
  RUN64 (uint32_t, uint64_t)                                                   \
  RUN64 (uint32_t, uint32_t)                                                   \
  RUN64 (uint32_t, uint16_t)                                                   \
  RUN64 (uint32_t, uint8_t)                                                    \
  RUN64 (uint32_t, int64_t)                                                    \
  RUN64 (uint32_t, int32_t)                                                    \
  RUN64 (uint32_t, int16_t)                                                    \
  RUN64 (uint32_t, int8_t)                                                     \
  RUN64N (int32_t, uint64_t)                                                    \
  RUN64N (int32_t, uint32_t)                                                    \
  RUN64N (int32_t, uint16_t)                                                    \
  RUN64N (int32_t, uint8_t)                                                     \
  RUN64N (int32_t, int64_t)                                                     \
  RUN64N (int32_t, int32_t)                                                     \
  RUN64N (int32_t, int16_t)                                                     \
  RUN64N (int32_t, int8_t)                                                      \
  RUN64 (uint16_t, uint64_t)                                                   \
  RUN64 (uint16_t, uint32_t)                                                   \
  RUN64 (uint16_t, uint16_t)                                                   \
  RUN64 (uint16_t, uint8_t)                                                    \
  RUN64 (uint16_t, int64_t)                                                    \
  RUN64 (uint16_t, int32_t)                                                    \
  RUN64 (uint16_t, int16_t)                                                    \
  RUN64 (uint16_t, int8_t)                                                     \
  RUN64N (int16_t, uint64_t)                                                    \
  RUN64N (int16_t, uint32_t)                                                    \
  RUN64N (int16_t, uint16_t)                                                    \
  RUN64N (int16_t, uint8_t)                                                     \
  RUN64N (int16_t, int64_t)                                                     \
  RUN64N (int16_t, int32_t)                                                     \
  RUN64N (int16_t, int16_t)                                                     \
  RUN64N (int16_t, int8_t)                                                      \
  RUN64 (uint8_t, uint64_t)                                                    \
  RUN64 (uint8_t, uint32_t)                                                    \
  RUN64 (uint8_t, uint16_t)                                                    \
  RUN64 (uint8_t, uint8_t)                                                     \
  RUN64 (uint8_t, int64_t)                                                     \
  RUN64 (uint8_t, int32_t)                                                     \
  RUN64 (uint8_t, int16_t)                                                     \
  RUN64 (uint8_t, int8_t)                                                      \
  RUN64N (int8_t, uint64_t)                                                     \
  RUN64N (int8_t, uint32_t)                                                     \
  RUN64N (int8_t, uint16_t)                                                     \
  RUN64N (int8_t, uint8_t)                                                      \
  RUN64N (int8_t, int64_t)                                                      \
  RUN64N (int8_t, int32_t)                                                      \
  RUN64N (int8_t, int16_t)                                                      \
  RUN64N (int8_t, int8_t)                                                       \
  RUN32 (uint64_t, uint64_t)                                                   \
  RUN32 (uint64_t, uint32_t)                                                   \
  RUN32 (uint64_t, uint16_t)                                                   \
  RUN32 (uint64_t, uint8_t)                                                    \
  RUN32 (uint64_t, int64_t)                                                    \
  RUN32 (uint64_t, int32_t)                                                    \
  RUN32 (uint64_t, int16_t)                                                    \
  RUN32 (uint64_t, int8_t)                                                     \
  RUN32N (int64_t, uint64_t)                                                    \
  RUN32N (int64_t, uint32_t)                                                    \
  RUN32N (int64_t, uint16_t)                                                    \
  RUN32N (int64_t, uint8_t)                                                     \
  RUN32N (int64_t, int64_t)                                                     \
  RUN32N (int64_t, int32_t)                                                     \
  RUN32N (int64_t, int16_t)                                                     \
  RUN32N (int64_t, int8_t)                                                      \
  RUN32 (uint32_t, uint64_t)                                                   \
  RUN32 (uint32_t, uint32_t)                                                   \
  RUN32 (uint32_t, uint16_t)                                                   \
  RUN32 (uint32_t, uint8_t)                                                    \
  RUN32 (uint32_t, int64_t)                                                    \
  RUN32 (uint32_t, int32_t)                                                    \
  RUN32 (uint32_t, int16_t)                                                    \
  RUN32 (uint32_t, int8_t)                                                     \
  RUN32N (int32_t, uint64_t)                                                    \
  RUN32N (int32_t, uint32_t)                                                    \
  RUN32N (int32_t, uint16_t)                                                    \
  RUN32N (int32_t, uint8_t)                                                     \
  RUN32N (int32_t, int64_t)                                                     \
  RUN32N (int32_t, int32_t)                                                     \
  RUN32N (int32_t, int16_t)                                                     \
  RUN32N (int32_t, int8_t)                                                      \
  RUN32 (uint16_t, uint64_t)                                                   \
  RUN32 (uint16_t, uint32_t)                                                   \
  RUN32 (uint16_t, uint16_t)                                                   \
  RUN32 (uint16_t, uint8_t)                                                    \
  RUN32 (uint16_t, int64_t)                                                    \
  RUN32 (uint16_t, int32_t)                                                    \
  RUN32 (uint16_t, int16_t)                                                    \
  RUN32 (uint16_t, int8_t)                                                     \
  RUN32N (int16_t, uint64_t)                                                    \
  RUN32N (int16_t, uint32_t)                                                    \
  RUN32N (int16_t, uint16_t)                                                    \
  RUN32N (int16_t, uint8_t)                                                     \
  RUN32N (int16_t, int64_t)                                                     \
  RUN32N (int16_t, int32_t)                                                     \
  RUN32N (int16_t, int16_t)                                                     \
  RUN32N (int16_t, int8_t)                                                      \
  RUN32 (uint8_t, uint64_t)                                                    \
  RUN32 (uint8_t, uint32_t)                                                    \
  RUN32 (uint8_t, uint16_t)                                                    \
  RUN32 (uint8_t, uint8_t)                                                     \
  RUN32 (uint8_t, int64_t)                                                     \
  RUN32 (uint8_t, int32_t)                                                     \
  RUN32 (uint8_t, int16_t)                                                     \
  RUN32 (uint8_t, int8_t)                                                      \
  RUN32N (int8_t, uint64_t)                                                     \
  RUN32N (int8_t, uint32_t)                                                     \
  RUN32N (int8_t, uint16_t)                                                     \
  RUN32N (int8_t, uint8_t)                                                      \
  RUN32N (int8_t, int64_t)                                                      \
  RUN32N (int8_t, int32_t)                                                      \
  RUN32N (int8_t, int16_t)                                                      \
  RUN32N (int8_t, int8_t)                                                       \
  RUNCTZ64 (uint64_t, uint64_t)                                                \
  RUNCTZ64 (uint64_t, uint32_t)                                                \
  RUNCTZ64 (uint64_t, uint16_t)                                                \
  RUNCTZ64 (uint64_t, uint8_t)                                                 \
  RUNCTZ64 (uint64_t, int64_t)                                                 \
  RUNCTZ64 (uint64_t, int32_t)                                                 \
  RUNCTZ64 (uint64_t, int16_t)                                                 \
  RUNCTZ64 (uint64_t, int8_t)                                                  \
  RUNCTZ64N (int64_t, uint64_t)                                                 \
  RUNCTZ64N (int64_t, uint32_t)                                                 \
  RUNCTZ64N (int64_t, uint16_t)                                                 \
  RUNCTZ64N (int64_t, uint8_t)                                                  \
  RUNCTZ64N (int64_t, int64_t)                                                  \
  RUNCTZ64N (int64_t, int32_t)                                                  \
  RUNCTZ64N (int64_t, int16_t)                                                  \
  RUNCTZ64N (int64_t, int8_t)                                                   \
  RUNCTZ64 (uint32_t, uint64_t)                                                \
  RUNCTZ64 (uint32_t, uint32_t)                                                \
  RUNCTZ64 (uint32_t, uint16_t)                                                \
  RUNCTZ64 (uint32_t, uint8_t)                                                 \
  RUNCTZ64 (uint32_t, int64_t)                                                 \
  RUNCTZ64 (uint32_t, int32_t)                                                 \
  RUNCTZ64 (uint32_t, int16_t)                                                 \
  RUNCTZ64 (uint32_t, int8_t)                                                  \
  RUNCTZ64N (int32_t, uint64_t)                                                 \
  RUNCTZ64N (int32_t, uint32_t)                                                 \
  RUNCTZ64N (int32_t, uint16_t)                                                 \
  RUNCTZ64N (int32_t, uint8_t)                                                  \
  RUNCTZ64N (int32_t, int64_t)                                                  \
  RUNCTZ64N (int32_t, int32_t)                                                  \
  RUNCTZ64N (int32_t, int16_t)                                                  \
  RUNCTZ64N (int32_t, int8_t)                                                   \
  RUNCTZ64 (uint16_t, uint64_t)                                                \
  RUNCTZ64 (uint16_t, uint32_t)                                                \
  RUNCTZ64 (uint16_t, uint16_t)                                                \
  RUNCTZ64 (uint16_t, uint8_t)                                                 \
  RUNCTZ64 (uint16_t, int64_t)                                                 \
  RUNCTZ64 (uint16_t, int32_t)                                                 \
  RUNCTZ64 (uint16_t, int16_t)                                                 \
  RUNCTZ64 (uint16_t, int8_t)                                                  \
  RUNCTZ64N (int16_t, uint64_t)                                                \
  RUNCTZ64N (int16_t, uint32_t)                                                \
  RUNCTZ64N (int16_t, uint16_t)                                                \
  RUNCTZ64N (int16_t, uint8_t)                                                 \
  RUNCTZ64N (int16_t, int64_t)                                                 \
  RUNCTZ64N (int16_t, int32_t)                                                 \
  RUNCTZ64N (int16_t, int16_t)                                                 \
  RUNCTZ64N (int16_t, int8_t)                                                  \
  RUNCTZ64 (uint8_t, uint64_t)                                                 \
  RUNCTZ64 (uint8_t, uint32_t)                                                 \
  RUNCTZ64 (uint8_t, uint16_t)                                                 \
  RUNCTZ64 (uint8_t, uint8_t)                                                  \
  RUNCTZ64 (uint8_t, int64_t)                                                  \
  RUNCTZ64 (uint8_t, int32_t)                                                  \
  RUNCTZ64 (uint8_t, int16_t)                                                  \
  RUNCTZ64 (uint8_t, int8_t)                                                   \
  RUNCTZ64N (int8_t, uint64_t)                                                 \
  RUNCTZ64N (int8_t, uint32_t)                                                 \
  RUNCTZ64N (int8_t, uint16_t)                                                 \
  RUNCTZ64N (int8_t, uint8_t)                                                  \
  RUNCTZ64N (int8_t, int64_t)                                                  \
  RUNCTZ64N (int8_t, int32_t)                                                  \
  RUNCTZ64N (int8_t, int16_t)                                                  \
  RUNCTZ64N (int8_t, int8_t)                                                   \
  RUNCTZ32 (uint64_t, uint64_t)                                                \
  RUNCTZ32 (uint64_t, uint32_t)                                                \
  RUNCTZ32 (uint64_t, uint16_t)                                                \
  RUNCTZ32 (uint64_t, uint8_t)                                                 \
  RUNCTZ32 (uint64_t, int64_t)                                                 \
  RUNCTZ32 (uint64_t, int32_t)                                                 \
  RUNCTZ32 (uint64_t, int16_t)                                                 \
  RUNCTZ32 (uint64_t, int8_t)                                                  \
  RUNCTZ32N (int64_t, uint64_t)                                                \
  RUNCTZ32N (int64_t, uint32_t)                                                \
  RUNCTZ32N (int64_t, uint16_t)                                                \
  RUNCTZ32N (int64_t, uint8_t)                                                 \
  RUNCTZ32N (int64_t, int64_t)                                                 \
  RUNCTZ32N (int64_t, int32_t)                                                 \
  RUNCTZ32N (int64_t, int16_t)                                                 \
  RUNCTZ32N (int64_t, int8_t)                                                  \
  RUNCTZ32 (uint32_t, uint64_t)                                                \
  RUNCTZ32 (uint32_t, uint32_t)                                                \
  RUNCTZ32 (uint32_t, uint16_t)                                                \
  RUNCTZ32 (uint32_t, uint8_t)                                                 \
  RUNCTZ32 (uint32_t, int64_t)                                                 \
  RUNCTZ32 (uint32_t, int32_t)                                                 \
  RUNCTZ32 (uint32_t, int16_t)                                                 \
  RUNCTZ32 (uint32_t, int8_t)                                                  \
  RUNCTZ32N (int32_t, uint64_t)                                                \
  RUNCTZ32N (int32_t, uint32_t)                                                \
  RUNCTZ32N (int32_t, uint16_t)                                                \
  RUNCTZ32N (int32_t, uint8_t)                                                 \
  RUNCTZ32N (int32_t, int64_t)                                                 \
  RUNCTZ32N (int32_t, int32_t)                                                 \
  RUNCTZ32N (int32_t, int16_t)                                                 \
  RUNCTZ32N (int32_t, int8_t)                                                  \
  RUNCTZ32 (uint16_t, uint64_t)                                                \
  RUNCTZ32 (uint16_t, uint32_t)                                                \
  RUNCTZ32 (uint16_t, uint16_t)                                                \
  RUNCTZ32 (uint16_t, uint8_t)                                                 \
  RUNCTZ32 (uint16_t, int64_t)                                                 \
  RUNCTZ32 (uint16_t, int32_t)                                                 \
  RUNCTZ32 (uint16_t, int16_t)                                                 \
  RUNCTZ32 (uint16_t, int8_t)                                                  \
  RUNCTZ32N (int16_t, uint64_t)                                                \
  RUNCTZ32N (int16_t, uint32_t)                                                \
  RUNCTZ32N (int16_t, uint16_t)                                                \
  RUNCTZ32N (int16_t, uint8_t)                                                 \
  RUNCTZ32N (int16_t, int64_t)                                                 \
  RUNCTZ32N (int16_t, int32_t)                                                 \
  RUNCTZ32N (int16_t, int16_t)                                                 \
  RUNCTZ32N (int16_t, int8_t)                                                  \
  RUNCTZ32 (uint8_t, uint64_t)                                                 \
  RUNCTZ32 (uint8_t, uint32_t)                                                 \
  RUNCTZ32 (uint8_t, uint16_t)                                                 \
  RUNCTZ32 (uint8_t, uint8_t)                                                  \
  RUNCTZ32 (uint8_t, int64_t)                                                  \
  RUNCTZ32 (uint8_t, int32_t)                                                  \
  RUNCTZ32 (uint8_t, int16_t)                                                  \
  RUNCTZ32 (uint8_t, int8_t)                                                   \
  RUNCTZ32N (int8_t, uint64_t)                                                 \
  RUNCTZ32N (int8_t, uint32_t)                                                 \
  RUNCTZ32N (int8_t, uint16_t)                                                 \
  RUNCTZ32N (int8_t, uint8_t)                                                  \
  RUNCTZ32N (int8_t, int64_t)                                                  \
  RUNCTZ32N (int8_t, int32_t)                                                  \
  RUNCTZ32N (int8_t, int16_t)                                                  \
  RUNCTZ32N (int8_t, int8_t)                                                   \
  RUNFFS64 (uint64_t, uint64_t)                                                \
  RUNFFS64 (uint64_t, uint32_t)                                                \
  RUNFFS64 (uint64_t, uint16_t)                                                \
  RUNFFS64 (uint64_t, uint8_t)                                                 \
  RUNFFS64 (uint64_t, int64_t)                                                 \
  RUNFFS64 (uint64_t, int32_t)                                                 \
  RUNFFS64 (uint64_t, int16_t)                                                 \
  RUNFFS64 (uint64_t, int8_t)                                                  \
  RUNFFS64N (int64_t, uint64_t)                                                \
  RUNFFS64N (int64_t, uint32_t)                                                \
  RUNFFS64N (int64_t, uint16_t)                                                \
  RUNFFS64N (int64_t, uint8_t)                                                 \
  RUNFFS64N (int64_t, int64_t)                                                 \
  RUNFFS64N (int64_t, int32_t)                                                 \
  RUNFFS64N (int64_t, int16_t)                                                 \
  RUNFFS64N (int64_t, int8_t)                                                  \
  RUNFFS64 (uint32_t, uint64_t)                                                \
  RUNFFS64 (uint32_t, uint32_t)                                                \
  RUNFFS64 (uint32_t, uint16_t)                                                \
  RUNFFS64 (uint32_t, uint8_t)                                                 \
  RUNFFS64 (uint32_t, int64_t)                                                 \
  RUNFFS64 (uint32_t, int32_t)                                                 \
  RUNFFS64 (uint32_t, int16_t)                                                 \
  RUNFFS64 (uint32_t, int8_t)                                                  \
  RUNFFS64N (int32_t, uint64_t)                                                \
  RUNFFS64N (int32_t, uint32_t)                                                \
  RUNFFS64N (int32_t, uint16_t)                                                \
  RUNFFS64N (int32_t, uint8_t)                                                 \
  RUNFFS64N (int32_t, int64_t)                                                 \
  RUNFFS64N (int32_t, int32_t)                                                 \
  RUNFFS64N (int32_t, int16_t)                                                 \
  RUNFFS64N (int32_t, int8_t)                                                  \
  RUNFFS64 (uint16_t, uint64_t)                                                \
  RUNFFS64 (uint16_t, uint32_t)                                                \
  RUNFFS64 (uint16_t, uint16_t)                                                \
  RUNFFS64 (uint16_t, uint8_t)                                                 \
  RUNFFS64 (uint16_t, int64_t)                                                 \
  RUNFFS64 (uint16_t, int32_t)                                                 \
  RUNFFS64 (uint16_t, int16_t)                                                 \
  RUNFFS64 (uint16_t, int8_t)                                                  \
  RUNFFS64N (int16_t, uint64_t)                                                \
  RUNFFS64N (int16_t, uint32_t)                                                \
  RUNFFS64N (int16_t, uint16_t)                                                \
  RUNFFS64N (int16_t, uint8_t)                                                 \
  RUNFFS64N (int16_t, int64_t)                                                 \
  RUNFFS64N (int16_t, int32_t)                                                 \
  RUNFFS64N (int16_t, int16_t)                                                 \
  RUNFFS64N (int16_t, int8_t)                                                  \
  RUNFFS64 (uint8_t, uint64_t)                                                 \
  RUNFFS64 (uint8_t, uint32_t)                                                 \
  RUNFFS64 (uint8_t, uint16_t)                                                 \
  RUNFFS64 (uint8_t, uint8_t)                                                  \
  RUNFFS64 (uint8_t, int64_t)                                                  \
  RUNFFS64 (uint8_t, int32_t)                                                  \
  RUNFFS64 (uint8_t, int16_t)                                                  \
  RUNFFS64 (uint8_t, int8_t)                                                   \
  RUNFFS64N (int8_t, uint64_t)                                                 \
  RUNFFS64N (int8_t, uint32_t)                                                 \
  RUNFFS64N (int8_t, uint16_t)                                                 \
  RUNFFS64N (int8_t, uint8_t)                                                  \
  RUNFFS64N (int8_t, int64_t)                                                  \
  RUNFFS64N (int8_t, int32_t)                                                  \
  RUNFFS64N (int8_t, int16_t)                                                  \
  RUNFFS64N (int8_t, int8_t)                                                   \
  RUNFFS32 (uint64_t, uint64_t)                                                \
  RUNFFS32 (uint64_t, uint32_t)                                                \
  RUNFFS32 (uint64_t, uint16_t)                                                \
  RUNFFS32 (uint64_t, uint8_t)                                                 \
  RUNFFS32 (uint64_t, int64_t)                                                 \
  RUNFFS32 (uint64_t, int32_t)                                                 \
  RUNFFS32 (uint64_t, int16_t)                                                 \
  RUNFFS32 (uint64_t, int8_t)                                                  \
  RUNFFS32N (int64_t, uint64_t)                                                \
  RUNFFS32N (int64_t, uint32_t)                                                \
  RUNFFS32N (int64_t, uint16_t)                                                \
  RUNFFS32N (int64_t, uint8_t)                                                 \
  RUNFFS32N (int64_t, int64_t)                                                 \
  RUNFFS32N (int64_t, int32_t)                                                 \
  RUNFFS32N (int64_t, int16_t)                                                 \
  RUNFFS32N (int64_t, int8_t)                                                  \
  RUNFFS32 (uint32_t, uint64_t)                                                \
  RUNFFS32 (uint32_t, uint32_t)                                                \
  RUNFFS32 (uint32_t, uint16_t)                                                \
  RUNFFS32 (uint32_t, uint8_t)                                                 \
  RUNFFS32 (uint32_t, int64_t)                                                 \
  RUNFFS32 (uint32_t, int32_t)                                                 \
  RUNFFS32 (uint32_t, int16_t)                                                 \
  RUNFFS32 (uint32_t, int8_t)                                                  \
  RUNFFS32N (int32_t, uint64_t)                                                \
  RUNFFS32N (int32_t, uint32_t)                                                \
  RUNFFS32N (int32_t, uint16_t)                                                \
  RUNFFS32N (int32_t, uint8_t)                                                 \
  RUNFFS32N (int32_t, int64_t)                                                 \
  RUNFFS32N (int32_t, int32_t)                                                 \
  RUNFFS32N (int32_t, int16_t)                                                 \
  RUNFFS32N (int32_t, int8_t)                                                  \
  RUNFFS32 (uint16_t, uint64_t)                                                \
  RUNFFS32 (uint16_t, uint32_t)                                                \
  RUNFFS32 (uint16_t, uint16_t)                                                \
  RUNFFS32 (uint16_t, uint8_t)                                                 \
  RUNFFS32 (uint16_t, int64_t)                                                 \
  RUNFFS32 (uint16_t, int32_t)                                                 \
  RUNFFS32 (uint16_t, int16_t)                                                 \
  RUNFFS32 (uint16_t, int8_t)                                                  \
  RUNFFS32N (int16_t, uint64_t)                                                \
  RUNFFS32N (int16_t, uint32_t)                                                \
  RUNFFS32N (int16_t, uint16_t)                                                \
  RUNFFS32N (int16_t, uint8_t)                                                 \
  RUNFFS32N (int16_t, int64_t)                                                 \
  RUNFFS32N (int16_t, int32_t)                                                 \
  RUNFFS32N (int16_t, int16_t)                                                 \
  RUNFFS32N (int16_t, int8_t)                                                  \
  RUNFFS32 (uint8_t, uint64_t)                                                 \
  RUNFFS32 (uint8_t, uint32_t)                                                 \
  RUNFFS32 (uint8_t, uint16_t)                                                 \
  RUNFFS32 (uint8_t, uint8_t)                                                  \
  RUNFFS32 (uint8_t, int64_t)                                                  \
  RUNFFS32 (uint8_t, int32_t)                                                  \
  RUNFFS32 (uint8_t, int16_t)                                                  \
  RUNFFS32 (uint8_t, int8_t)                                                   \
  RUNFFS32N (int8_t, uint64_t)                                                 \
  RUNFFS32N (int8_t, uint32_t)                                                 \
  RUNFFS32N (int8_t, uint16_t)                                                 \
  RUNFFS32N (int8_t, uint8_t)                                                  \
  RUNFFS32N (int8_t, int64_t)                                                  \
  RUNFFS32N (int8_t, int32_t)                                                  \
  RUNFFS32N (int8_t, int16_t)                                                  \
  RUNFFS32N (int8_t, int8_t)

int
main ()
{
  RUN_ALL ()
}

/* TODO: Due to an over-zealous check in tree-vect-patterns we do not vectorize
   e.g.
     uint64_t dst[];
     uint32_t src[];
     dst[i] = __builtin_popcountll (src[i]);
   even though we could.  Therefore, for now, adjust the following checks.
   This difference was exposed in r14-5557-g6dd4c703be17fa.  */
/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 229 "vect" { target { { rv64 } && { ! riscv_zbb } } } } } */
/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 250 "vect" { target { { rv32 } || { riscv_zbb } } } } } */
