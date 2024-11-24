/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Oz" "-Os" "-flto" } } */
/* { dg-options "-march=rv64gc_xtheadmempair -mtune=thead-c906" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_xtheadmempair -mtune=thead-c906" { target { rv32 } } } */

#include <inttypes.h>

#if __riscv_xlen == 32
typedef uint32_t xlen_t;
#else
typedef uint64_t xlen_t;
#endif

void foof (xlen_t*, xlen_t, xlen_t);
void foor (xlen_t*, xlen_t, xlen_t);
void foowu (uint32_t*, uint64_t, uint64_t);
void foows (int32_t*, int64_t, int64_t);

#define LxD_TEST(f, T, i1, i2)		\
void					\
f ## i1 ## i2(T *arr)			\
{					\
  foo ## f(arr, arr[i1], arr[i2]);	\
}

// works
LxD_TEST(f, xlen_t, 0, 1)
// does not work (can't merge with unaligned offset)
LxD_TEST(f, xlen_t, 1, 2)
// works
LxD_TEST(f, xlen_t, 2, 3)
// does not work (can't merge with unaligned offset)
LxD_TEST(f, xlen_t, 3, 4)
// works
LxD_TEST(f, xlen_t, 4, 5)
// does not work (can't merge with unaligned offset)
LxD_TEST(f, xlen_t, 5, 6)
// works
LxD_TEST(f, xlen_t, 6, 7)
// does not work (can't merge with unaligned offset)
LxD_TEST(f, xlen_t, 7, 8)
// does not work (out of range)
LxD_TEST(f, xlen_t, 8, 9)

// works with reordering
LxD_TEST(r, xlen_t, 1, 0)
// does not work (can't merge with unaligned offset)
LxD_TEST(r, xlen_t, 2, 1)
// works with reordering
LxD_TEST(r, xlen_t, 3, 2)
// does not work (can't merge with unaligned offset)
LxD_TEST(r, xlen_t, 4, 3)
// works with reordering
LxD_TEST(r, xlen_t, 5, 4)
// does not work (can't merge with unaligned offset)
LxD_TEST(r, xlen_t, 6, 5)
// works with reordering
LxD_TEST(r, xlen_t, 7, 6)
// does not work (can't merge with unaligned offset)
LxD_TEST(r, xlen_t, 8, 7)
// does not work (out of range)
LxD_TEST(r, xlen_t, 9, 8)

#if __riscv_xlen != 32
// works
LxD_TEST(wu, uint32_t, 0, 1)
LxD_TEST(ws, int32_t, 0, 1)
// does not work (can't merge with unaligned offset)
LxD_TEST(wu, uint32_t, 1, 2)
LxD_TEST(ws, int32_t, 1, 2)
// works
LxD_TEST(wu, uint32_t, 2, 3)
LxD_TEST(ws, int32_t, 2, 3)
// does not work (can't merge with unaligned offset)
LxD_TEST(wu, uint32_t, 3, 4)
LxD_TEST(ws, int32_t, 3, 4)
// works
LxD_TEST(wu, uint32_t, 4, 5)
LxD_TEST(ws, int32_t, 4, 5)
// does not work (can't merge with unaligned offset)
LxD_TEST(wu, uint32_t, 5, 6)
LxD_TEST(ws, int32_t, 5, 6)
// works
LxD_TEST(wu, uint32_t, 6, 7)
LxD_TEST(ws, int32_t, 6, 7)
// does not work (can't merge with unaligned offset)
LxD_TEST(wu, uint32_t, 7, 8)
LxD_TEST(ws, int32_t, 7, 8)
// does not work (out of range)
LxD_TEST(wu, uint32_t, 8, 9)
LxD_TEST(ws, int32_t, 8, 9)
#endif

/* { dg-final { scan-assembler-times "th.ldd\t" 8 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "th.lwud\t" 4 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "th.lwd\t" 4 { target { rv64 } } } } */

/* { dg-final { scan-assembler-times "th.lwd\t" 8 { target { rv32 } } } } */
