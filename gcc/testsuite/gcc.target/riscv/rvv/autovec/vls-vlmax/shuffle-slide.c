/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mrvv-max-lmul=m8 -Wno-overflow" } */

#include "perm.h"

#define SERIES_2(x, y) (x), (x + 1)
#define SERIES_4(x, y) SERIES_2 (x, y), SERIES_2 (x + 2, y)
#define SERIES_8(x, y) SERIES_4 (x, y), SERIES_4 (x + 4, y)
#define SERIES_16(x, y) SERIES_8 (x, y), SERIES_8 (x + 8, y)
#define SERIES_32(x, y) SERIES_16 (x, y), SERIES_16 (x + 16, y)
#define SERIES_64(x, y) SERIES_32 (x, y), SERIES_32 (x + 32, y)
#define SERIES_128(x, y) SERIES_64 (x, y), SERIES_64 (x + 64, y)

#define MASK1_4(X, Y) SERIES_2 (X, Y), SERIES_2 (X + 4, Y)
#define MASK1_8(X, Y) SERIES_4 (X, Y), SERIES_4 (X + 8, Y)
#define MASK1_16(X, Y) SERIES_8 (X, Y), SERIES_8 (X + 16, Y)
#define MASK1_32(X, Y) SERIES_16 (X, Y), SERIES_16 (X + 32, Y)
#define MASK1_64(X, Y) SERIES_32 (X, Y), SERIES_32 (X + 64, Y)
#define MASK1_128(X, Y) SERIES_64 (X, Y), SERIES_64 (X + 128, Y)

#define MASK1D_4(X, Y) SERIES_2 (X + 2, Y), SERIES_2 (X + 6, Y)
#define MASK1D_8(X, Y) SERIES_4 (X + 4, Y), SERIES_4 (X + 12, Y)
#define MASK1D_16(X, Y) SERIES_8 (X + 8, Y), SERIES_8 (X + 24, Y)
#define MASK1D_32(X, Y) SERIES_16 (X + 16, Y), SERIES_16 (X + 48, Y)
#define MASK1D_64(X, Y) SERIES_32 (X + 32, Y), SERIES_32 (X + 96, Y)
#define MASK1D_128(X, Y) SERIES_64 (X + 64, Y), SERIES_64 (X + 192, Y)

#define MASK2U_4(X, Y) 0, 1, 2, 4
#define MASK2U_8(X, Y) 0, 1, 2, 3, 4, 5, 6, 8
#define MASK2U_16(X, Y) 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16
#define MASK2U_32(X, Y)                                                        \
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,    \
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 32
#define MASK2U_64(X, Y)                                                        \
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,    \
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,    \
    39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,    \
    57, 58, 59, 60, 61, 62, 64
#define MASK2U_128(X, Y)                                                       \
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,    \
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,    \
    39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56,    \
    57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74,    \
    75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92,    \
    93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108,   \
    109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, \
    124, 125, 126, 128

#define MASK3U_4(X, Y) 0, 4, 5, 6
#define MASK3U_8(X, Y) 0, 8, 9, 10, 11, 12, 13, 14
#define MASK3U_16(X, Y)                                                        \
  0, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30
#define MASK3U_32(X, Y)                                                        \
  0, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,   \
    50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62
#define MASK3U_64(X, Y)                                                        \
  0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,   \
    82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,    \
    100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, \
    115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126
#define MASK3U_128(X, Y)                                                       \
  0, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141,     \
    142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, \
    157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, \
    172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, \
    187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, \
    202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, \
    217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, \
    232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, \
    247, 248, 249, 250, 251, 252, 253, 254

#define MASK2D_4(X, Y) 1, 2, 3, 7
#define MASK2D_8(X, Y) 1, 2, 3, 4, 5, 6, 7, 15
#define MASK2D_16(X, Y) 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 31
#define MASK2D_32(X, Y)                                                        \
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,   \
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 63
#define MASK2D_64(X, Y)                                                        \
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,   \
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,    \
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,    \
    58, 59, 60, 61, 62, 63, 127
#define MASK2D_128(X, Y)                                                       \
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,   \
    22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39,    \
    40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,    \
    58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75,    \
    76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93,    \
    94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,  \
    110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, \
    125, 126, 127, 255

#define MASK3D_4(X, Y) 3, 5, 6, 7
#define MASK3D_8(X, Y) 7, 9, 10, 11, 12, 13, 14, 15
#define MASK3D_16(X, Y)                                                        \
  15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
#define MASK3D_32(X, Y)                                                        \
  31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,  \
    51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63
#define MASK3D_64(X, Y)                                                        \
  63, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,  \
    83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,   \
    101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, \
    116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127
#define MASK3D_128(X, Y)                                                       \
  127, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142,   \
    143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, \
    158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, \
    173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, \
    188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, \
    203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, \
    218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, \
    233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, \
    248, 249, 250, 251, 252, 253, 254, 255

#define PERMUTE1(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute1_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK1_##NUNITS (0, NUNITS));             \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE2(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute2_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK1D_##NUNITS (0, NUNITS));            \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE3(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute3_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK2U_##NUNITS (0, NUNITS));            \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE4(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute4_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK3U_##NUNITS (0, NUNITS));            \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE5(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute5_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK2D_##NUNITS (0, NUNITS));            \
    *(TYPE *) out = v;                                                         \
  }

#define PERMUTE6(TYPE, NUNITS)                                                 \
  __attribute__ ((noipa)) void permute6_##TYPE (TYPE values1, TYPE values2,    \
						TYPE *out)                     \
  {                                                                            \
    TYPE v = __builtin_shufflevector (values1, values2,                        \
				      MASK3D_##NUNITS (0, NUNITS));            \
    *(TYPE *) out = v;                                                         \
  }

#define TEST_ALL(T)                                                            \
  T (vnx4qi, 4)                                                                \
  T (vnx8qi, 8)                                                                \
  T (vnx16qi, 16)                                                              \
  T (vnx32qi, 32)                                                              \
  T (vnx64qi, 64)                                                              \
  T (vnx128qi, 128)                                                            \
  T (vnx4hi, 4)                                                                \
  T (vnx8hi, 8)                                                                \
  T (vnx16hi, 16)                                                              \
  T (vnx32hi, 32)                                                              \
  T (vnx64hi, 64)                                                              \
  T (vnx4si, 4)                                                                \
  T (vnx8si, 8)                                                                \
  T (vnx16si, 16)                                                              \
  T (vnx32si, 32)                                                              \
  T (vnx4di, 4)                                                                \
  T (vnx8di, 8)                                                                \
  T (vnx16di, 16)                                                              \
  T (vnx4sf, 4)                                                                \
  T (vnx8sf, 8)                                                                \
  T (vnx16sf, 16)                                                              \
  T (vnx32sf, 32)                                                              \
  T (vnx4df, 4)                                                                \
  T (vnx8df, 8)                                                                \
  T (vnx16df, 16)

TEST_ALL (PERMUTE1)
TEST_ALL (PERMUTE2)
TEST_ALL (PERMUTE3)
TEST_ALL (PERMUTE4)
TEST_ALL (PERMUTE5)
TEST_ALL (PERMUTE6)

/* { dg-final { scan-assembler-times "vslideup" 75 } } */
/* { dg-final { scan-assembler-times "vslidedown" 75 } } */
/* { dg-final { scan-assembler-not "vrgather" } } */
/* { dg-final { scan-assembler-not "vmerge" } } */
