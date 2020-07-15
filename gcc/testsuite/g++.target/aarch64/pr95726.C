#include <arm_neon.h>

typedef float vecf __attribute__((vector_size(16)));

// This assertion must hold: vecf and float32x4_t have distinct identities
// and mangle differently, so they are not interchangeable.
template<typename T> struct bar;
template<> struct bar<vecf> { static const int x = 1; };
template<> struct bar<float32x4_t> { static const int x = 2; };
static_assert(bar<vecf>::x + bar<float32x4_t>::x == 3, "boo");

// GCC 10 and earlier should continue to accept this, but the behavior
// changed in GCC 11.
vecf x;
float32x4_t y;
float32x4_t &z = x;

// These assignment must be valid even in the strictest mode: vecf must
// implicitly convert to float32x4_t and vice versa.
void foo() { x = y; y = x; }

// GCC 10 and earlier should continue to accept this, but the behavior
// changed in GCC 11.
auto sel1(bool c, decltype(c ? x : y) d) { return d; }
auto sel2(bool c, decltype(c ? y : x) d) { return d; }

/* { dg-final { scan-assembler {_Z4sel1bRDv4_f} } } */
/* { dg-final { scan-assembler {_Z4sel2bR13__Float32x4_t} } } */
