// { dg-require-effective-target arm_neon_ok }
// { dg-add-options arm_neon }

#include <arm_neon.h>

typedef float vecf __attribute__((vector_size(16)));

// This assertion must hold: vecf and float32x4_t have distinct identities
// and mangle differently, so they are not interchangeable.
template<typename T> struct bar;
template<> struct bar<vecf> { static const int x = 1; };
template<> struct bar<float32x4_t> { static const int x = 2; };
static_assert(bar<vecf>::x + bar<float32x4_t>::x == 3, "boo");

// GCC 10.1 and earlier accepted this.  However, the rule should be
// that GNU vectors and Advanced SIMD vectors are distinct types but
// that each one implicitly converts to the other.  The types are not
// reference-compatible.
//
// The behavior tested below is consistent with Clang.
vecf x;
float32x4_t y;
float32x4_t &z = x; // { dg-error {cannot bind non-const lvalue reference} }

// These assignment must be valid even in the strictest mode: vecf must
// implicitly convert to float32x4_t and vice versa.
void foo() { x = y; y = x; }

// Previously GCC accepted this and took the type of "d" from the "then" arm.
// It therefore mangled the functions as:
//
//   _Z4sel1bRDv4_f
//   _Z4sel2bR19__simd128_float32_t
//
// Clang currently also accepts it and takes the type of "d" from the
// "else" arm.  It therefore mangles the functions as follows, which is
// inconsistent with the old GCC behavior:
//
//   _Z4sel1b19__simd128_float32_t
//   _Z4sel2bDv4_f
//
// Given that the types have distinct identities and that each one
// implicitly converts to the other (see above), the expression ought
// to be rejected as invalid.  This is consistent (by analogy) with the
// standard C++ handling of conditional expressions involving class types,
// in cases where the "then" value implicitly converts to the "else" type
// and the "else" value implicitly converts to the "then" type.
auto sel1(bool c, decltype(c ? x : y) d) { return d; } // { dg-error {operands to '\?:' have different types} }
auto sel2(bool c, decltype(c ? y : x) d) { return d; } // { dg-error {operands to '\?:' have different types} }
