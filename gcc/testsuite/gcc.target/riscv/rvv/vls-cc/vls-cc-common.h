#ifndef VLS_CC_COMMON_H
#define VLS_CC_COMMON_H

#include <stdint-gcc.h>

// Check if ABI_VLEN is defined
#ifndef ABI_VLEN
#define ABI_VLEN 128  // Default fallback value
#endif

// VLS Calling Convention attribute macro
#define VLS_CC(ABI_VLEN) \
  __attribute__((riscv_vls_cc(ABI_VLEN)))

// Fixed-length vector type definitions following <basetype><width>x<num>_t naming convention
// These use GCC/Clang __attribute__((vector_size(N))) extension

// 8-bit integer vectors
typedef int8_t int8x2_t __attribute__((vector_size(2)));
typedef int8_t int8x4_t __attribute__((vector_size(4)));
typedef int8_t int8x8_t __attribute__((vector_size(8)));
typedef int8_t int8x16_t __attribute__((vector_size(16)));
typedef int8_t int8x32_t __attribute__((vector_size(32)));
typedef int8_t int8x64_t __attribute__((vector_size(64)));

typedef uint8_t uint8x2_t __attribute__((vector_size(2)));
typedef uint8_t uint8x4_t __attribute__((vector_size(4)));
typedef uint8_t uint8x8_t __attribute__((vector_size(8)));
typedef uint8_t uint8x16_t __attribute__((vector_size(16)));
typedef uint8_t uint8x32_t __attribute__((vector_size(32)));
typedef uint8_t uint8x64_t __attribute__((vector_size(64)));

// 16-bit integer vectors
typedef int16_t int16x2_t __attribute__((vector_size(4)));
typedef int16_t int16x4_t __attribute__((vector_size(8)));
typedef int16_t int16x8_t __attribute__((vector_size(16)));
typedef int16_t int16x16_t __attribute__((vector_size(32)));
typedef int16_t int16x32_t __attribute__((vector_size(64)));

typedef uint16_t uint16x2_t __attribute__((vector_size(4)));
typedef uint16_t uint16x4_t __attribute__((vector_size(8)));
typedef uint16_t uint16x8_t __attribute__((vector_size(16)));
typedef uint16_t uint16x16_t __attribute__((vector_size(32)));
typedef uint16_t uint16x32_t __attribute__((vector_size(64)));

// 32-bit integer vectors
typedef int32_t int32x2_t __attribute__((vector_size(8)));
typedef int32_t int32x4_t __attribute__((vector_size(16)));
typedef int32_t int32x8_t __attribute__((vector_size(32)));
typedef int32_t int32x16_t __attribute__((vector_size(64)));

typedef uint32_t uint32x2_t __attribute__((vector_size(8)));
typedef uint32_t uint32x4_t __attribute__((vector_size(16)));
typedef uint32_t uint32x8_t __attribute__((vector_size(32)));
typedef uint32_t uint32x16_t __attribute__((vector_size(64)));

// 64-bit integer vectors
typedef int64_t int64x2_t __attribute__((vector_size(16)));
typedef int64_t int64x4_t __attribute__((vector_size(32)));
typedef int64_t int64x8_t __attribute__((vector_size(64)));

typedef uint64_t uint64x2_t __attribute__((vector_size(16)));
typedef uint64_t uint64x4_t __attribute__((vector_size(32)));
typedef uint64_t uint64x8_t __attribute__((vector_size(64)));

// Floating-point vectors (following the same pattern)
typedef float float32x2_t __attribute__((vector_size(8)));
typedef float float32x4_t __attribute__((vector_size(16)));
typedef float float32x8_t __attribute__((vector_size(32)));
typedef float float32x16_t __attribute__((vector_size(64)));

typedef double double64x2_t __attribute__((vector_size(16)));
typedef double double64x4_t __attribute__((vector_size(32)));
typedef double double64x8_t __attribute__((vector_size(64)));

// Test structures containing fixed-length vectors
typedef struct {
    int32x4_t vec1;
    int32x4_t vec2;
} struct_two_same_vectors_t;

typedef struct {
    int32x4_t vec;
} struct_single_vector_t;

typedef struct {
    int32x4_t vec_array[2];
} struct_vector_array_t;

typedef struct {
    int32x2_t vec1;
    int32x4_t vec2;  // Different sizes
} struct_different_vectors_t;

typedef struct {
    int32x4_t vec;
    int scalar;
} struct_mixed_t;

// Union containing fixed-length vectors (always uses integer calling convention)
typedef union {
    int32x4_t vec;
    int32_t scalars[4];
} union_vector_t;

// Function pointer types for testing
typedef int32x4_t (*func_return_vector_t)(int32x4_t);
typedef void (*func_pass_vector_t)(int32x4_t);
typedef struct_single_vector_t (*func_return_struct_t)(struct_single_vector_t);

// Test constants for initialization
#define INIT_INT32X4(a, b, c, d) (int32x4_t){a, b, c, d}
#define INIT_FLOAT32X4(a, b, c, d) (float32x4_t){a, b, c, d}
#define INIT_INT16X8(a, b, c, d, e, f, g, h) (int16x8_t){a, b, c, d, e, f, g, h}

#ifdef SUPPORT_NON_POWER_OF_2_VEC
#define INIT_INT32X3(a, b, c) (int32x3_t){a, b, c}
#define INIT_FLOAT32X3(a, b, c) (float32x3_t){a, b, c}
#endif

// Utility macros for vector operations testing
#define VEC_SIZE_BYTES(type) sizeof(type)
#define VEC_ELEMENT_COUNT(type, element_type) (sizeof(type) / sizeof(element_type))

#endif // VLS_CC_COMMON_H
