/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/*
   Test taken from LLVM
    compiler-rt/test/hwasan/TestCases/check-interface.cpp
 */
// Utilizes all flavors of __hwasan_load/store interface functions to verify
// that the instrumentation and the interface provided by HWASan do match.
// In case of a discrepancy, this test fails to link.

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

#define F(T) void f_##T(T *a, T *b) { *a = *b; }

F(uint8_t)
F(uint16_t)
F(uint32_t)
F(uint64_t)

typedef unsigned V32 __attribute__((__vector_size__(32)));
F(V32)

int main() {}
