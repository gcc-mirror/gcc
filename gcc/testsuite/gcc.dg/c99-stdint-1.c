/* Verify that <stdint.h> is present and follows the C99 requirements.
   If this test fails because of the header being missing on a
   particular target, this indicates GCC has not been correctly
   configured regarding what version of <stdint.h> to install or what
   the <stdint.h> types are on that target.  If GCC is wrapping a
   system copy of the header and some tests fail because of bugs in
   that copy, they should be fixed with fixincludes (and the bugs
   reported to maintainer of that copy if still present in the latest
   version).  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors -fhosted" } */
/* { dg-require-effective-target ptr32plus } */

#include <limits.h>
#include <stdint.h>
/* This and the later SIG_ATOMIC_* tests should be appropriately
   conditioned for any freestanding targets with no <signal.h>.  */
#ifndef SIGNAL_SUPPRESS
#include <signal.h>
#endif

/* Note that some of these conditions assume two's complement and no
   padding bits; GCC only supports two's complement, and no supported
   target has padding bits in any integer type of the standard
   widths.  */
#define CHECK_SIGNED(TYPE) \
  do { TYPE a; int b[(TYPE)-1 < 0 ? 1 : -1]; } while (0)
#define CHECK_UNSIGNED(TYPE) \
  do { TYPE a; int b[(TYPE)-1 < 0 ? -1 : 1]; } while (0)
#define CHECK_WIDTH_EQUALS(TYPE, WIDTH) \
  do { int a[sizeof(TYPE) * CHAR_BIT == (WIDTH) ? 1 : -1]; } while (0)
#define CHECK_WIDTH_AT_LEAST(TYPE, WIDTH) \
  do { int a[sizeof(TYPE) * CHAR_BIT >= (WIDTH) ? 1 : -1]; } while (0)
#define CHECK_WIDTH_ORDER(TYPE1, TYPE2) \
  do { int a[sizeof(TYPE2) >= sizeof(TYPE1) ? 1 : -1]; } while (0)
#define CHECK_EXPR_TYPE(TYPE, EXPR) \
  do { __typeof__(EXPR) a; __typeof__((TYPE)0 + 0) *b = &a; } while (0)
#define UNSIGNED_MAX_COND(TYPE, EXPR) \
  ((EXPR) == (TYPE)-1)
#define SIGNED_MIN_MAX_COND(TYPE, MIN, MAX)				\
  ((MIN) == -(MAX)-1							\
   && ((MAX) & 1)							\
   && ((((MAX) >> 1) + 1) >> (sizeof(TYPE) * CHAR_BIT - 2)) == 1)
#define MIN_MAX_COND(TYPE, MIN, MAX)			\
  ((TYPE)-1 < 0						\
   ? SIGNED_MIN_MAX_COND(TYPE, (MIN), (MAX))		\
   : ((MIN) == 0 && UNSIGNED_MAX_COND(TYPE, (MAX))))
#define CHECK_SIGNED_LIMITS(TYPE, MIN, MAX)				\
  CHECK_SIGNED(TYPE);							\
  CHECK_EXPR_TYPE(TYPE, (MIN));						\
  CHECK_EXPR_TYPE(TYPE, (MAX));						\
  do { int a[SIGNED_MIN_MAX_COND(TYPE, (MIN), (MAX)) ? 1 : -1]; } while (0)
#define CHECK_SIGNED_LIMITS_2(TYPE, MIN, MAX, MINBD, MAXBD)	\
  CHECK_SIGNED(TYPE);						\
  CHECK_EXPR_TYPE(TYPE, (MIN));					\
  CHECK_EXPR_TYPE(TYPE, (MAX));					\
  do { int a[(SIGNED_MIN_MAX_COND(TYPE, (MIN), (MAX))		\
	      && (MIN) <= (MINBD)				\
	      && (MAX) >= (MAXBD)) ? 1 : -1]; } while (0)
#define CHECK_UNSIGNED_LIMITS(TYPE, MAX)				\
  CHECK_UNSIGNED(TYPE);							\
  CHECK_EXPR_TYPE(TYPE, (MAX));						\
  do { int a[UNSIGNED_MAX_COND(TYPE, (MAX)) ? 1 : -1]; } while (0)
#define CHECK_UNSIGNED_LIMITS_2(TYPE, MAX, MAXBD)		\
  CHECK_UNSIGNED(TYPE);						\
  CHECK_EXPR_TYPE(TYPE, (MAX));					\
  do { int a[(UNSIGNED_MAX_COND(TYPE, (MAX))			\
	      && (MAX) >= (MAXBD)) ? 1 : -1]; } while (0)
#define CHECK_LIMITS_2(TYPE, MIN, MAX, SMINBD, SMAXBD, UMAXBD)	\
  do { int a[(MIN_MAX_COND(TYPE, (MIN), (MAX))			\
	      && ((TYPE)-1 < 0					\
		  ? ((MIN) <= (SMINBD) && (MAX) >= (SMAXBD))	\
		  : (MAX) >= (UMAXBD))) ? 1 : -1]; } while (0)
#define CHECK_CONSTS(TYPE, MACRO)				\
  CHECK_EXPR_TYPE(TYPE, MACRO(01));				\
  CHECK_EXPR_TYPE(TYPE, MACRO(2));				\
  CHECK_EXPR_TYPE(TYPE, MACRO(0x3));				\
  do { int a[(MACRO(12) == 12					\
	      && MACRO(012) == 012				\
	      && MACRO(0x12) == 0x12) ? 1 : -1]; } while (0);

void
test_exact (void)
{
#ifdef INT8_MIN
  CHECK_WIDTH_EQUALS(int8_t, 8);
  CHECK_SIGNED_LIMITS(int8_t, INT8_MIN, INT8_MAX);
#else
  CHECK_WIDTH_AT_LEAST(int_least8_t, 9);
#endif
#ifdef INT16_MIN
  CHECK_WIDTH_EQUALS(int16_t, 16);
  CHECK_SIGNED_LIMITS(int16_t, INT16_MIN, INT16_MAX);
#else
  CHECK_WIDTH_AT_LEAST(int_least16_t, 17);
#endif
#ifdef INT32_MIN
  CHECK_WIDTH_EQUALS(int32_t, 32);
  CHECK_SIGNED_LIMITS(int32_t, INT32_MIN, INT32_MAX);
#else
  CHECK_WIDTH_AT_LEAST(int_least32_t, 33);
#endif
#ifdef INT64_MIN
  CHECK_WIDTH_EQUALS(int64_t, 64);
  CHECK_SIGNED_LIMITS(int64_t, INT64_MIN, INT64_MAX);
#else
  CHECK_WIDTH_AT_LEAST(int_least64_t, 65);
#endif
#ifdef UINT8_MAX
  CHECK_WIDTH_EQUALS(uint8_t, 8);
  CHECK_UNSIGNED_LIMITS(uint8_t, UINT8_MAX);
#else
  CHECK_WIDTH_AT_LEAST(uint_least8_t, 9);
#endif
#ifdef UINT16_MAX
  CHECK_WIDTH_EQUALS(uint16_t, 16);
  CHECK_UNSIGNED_LIMITS(uint16_t, UINT16_MAX);
#else
  CHECK_WIDTH_AT_LEAST(uint_least16_t, 17);
#endif
#ifdef UINT32_MAX
  CHECK_WIDTH_EQUALS(uint32_t, 32);
  CHECK_UNSIGNED_LIMITS(uint32_t, UINT32_MAX);
#else
  CHECK_WIDTH_AT_LEAST(uint_least32_t, 33);
#endif
#ifdef UINT64_MAX
  CHECK_WIDTH_EQUALS(uint64_t, 64);
  CHECK_UNSIGNED_LIMITS(uint64_t, UINT64_MAX);
#else
  CHECK_WIDTH_AT_LEAST(uint_least64_t, 65);
#endif
}

void
test_least (void)
{
  CHECK_WIDTH_AT_LEAST(int_least8_t, 8);
  CHECK_WIDTH_ORDER(int_least8_t, int_fast8_t);
  CHECK_SIGNED_LIMITS(int_least8_t, INT_LEAST8_MIN, INT_LEAST8_MAX);
  CHECK_WIDTH_AT_LEAST(int_least16_t, 16);
  CHECK_WIDTH_ORDER(int_least16_t, int_fast16_t);
  CHECK_SIGNED_LIMITS(int_least16_t, INT_LEAST16_MIN, INT_LEAST16_MAX);
  CHECK_WIDTH_AT_LEAST(int_least32_t, 32);
  CHECK_WIDTH_ORDER(int_least32_t, int_fast32_t);
  CHECK_SIGNED_LIMITS(int_least32_t, INT_LEAST32_MIN, INT_LEAST32_MAX);
  CHECK_WIDTH_AT_LEAST(int_least64_t, 64);
  CHECK_WIDTH_ORDER(int_least64_t, int_fast64_t);
  CHECK_SIGNED_LIMITS(int_least64_t, INT_LEAST64_MIN, INT_LEAST64_MAX);
  CHECK_WIDTH_AT_LEAST(uint_least8_t, 8);
  CHECK_WIDTH_ORDER(uint_least8_t, uint_fast8_t);
  CHECK_UNSIGNED_LIMITS(uint_least8_t, UINT_LEAST8_MAX);
  CHECK_WIDTH_AT_LEAST(uint_least16_t, 16);
  CHECK_WIDTH_ORDER(uint_least16_t, uint_fast16_t);
  CHECK_UNSIGNED_LIMITS(uint_least16_t, UINT_LEAST16_MAX);
  CHECK_WIDTH_AT_LEAST(uint_least32_t, 32);
  CHECK_WIDTH_ORDER(uint_least32_t, uint_fast32_t);
  CHECK_UNSIGNED_LIMITS(uint_least32_t, UINT_LEAST32_MAX);
  CHECK_WIDTH_AT_LEAST(uint_least64_t, 64);
  CHECK_WIDTH_ORDER(uint_least64_t, uint_fast64_t);
  CHECK_UNSIGNED_LIMITS(uint_least64_t, UINT_LEAST64_MAX);
}

void
test_fast (void)
{
  CHECK_WIDTH_AT_LEAST(int_fast8_t, 8);
  CHECK_SIGNED_LIMITS(int_fast8_t, INT_FAST8_MIN, INT_FAST8_MAX);
  CHECK_WIDTH_AT_LEAST(int_fast16_t, 16);
  CHECK_SIGNED_LIMITS(int_fast16_t, INT_FAST16_MIN, INT_FAST16_MAX);
  CHECK_WIDTH_AT_LEAST(int_fast32_t, 32);
  CHECK_SIGNED_LIMITS(int_fast32_t, INT_FAST32_MIN, INT_FAST32_MAX);
  CHECK_WIDTH_AT_LEAST(int_fast64_t, 64);
  CHECK_SIGNED_LIMITS(int_fast64_t, INT_FAST64_MIN, INT_FAST64_MAX);
  CHECK_WIDTH_AT_LEAST(uint_fast8_t, 8);
  CHECK_UNSIGNED_LIMITS(uint_fast8_t, UINT_FAST8_MAX);
  CHECK_WIDTH_AT_LEAST(uint_fast16_t, 16);
  CHECK_UNSIGNED_LIMITS(uint_fast16_t, UINT_FAST16_MAX);
  CHECK_WIDTH_AT_LEAST(uint_fast32_t, 32);
  CHECK_UNSIGNED_LIMITS(uint_fast32_t, UINT_FAST32_MAX);
  CHECK_WIDTH_AT_LEAST(uint_fast64_t, 64);
  CHECK_UNSIGNED_LIMITS(uint_fast64_t, UINT_FAST64_MAX);
}

void
test_ptr (void)
{
#ifdef INTPTR_MIN
  CHECK_SIGNED_LIMITS_2(intptr_t, INTPTR_MIN, INTPTR_MAX, -0x7fff, 0x7fff);
#endif
#ifdef UINTPTR_MAX
  CHECK_UNSIGNED_LIMITS_2(uintptr_t, UINTPTR_MAX, 0xffffU);
#endif
}

void
test_max (void)
{
  CHECK_WIDTH_AT_LEAST(intmax_t, 64);
  CHECK_WIDTH_ORDER(long long, intmax_t);
  CHECK_WIDTH_ORDER(int_fast8_t, intmax_t);
  CHECK_WIDTH_ORDER(int_fast16_t, intmax_t);
  CHECK_WIDTH_ORDER(int_fast32_t, intmax_t);
  CHECK_WIDTH_ORDER(int_fast64_t, intmax_t);
  CHECK_SIGNED_LIMITS(intmax_t, INTMAX_MIN, INTMAX_MAX);
  CHECK_WIDTH_AT_LEAST(uintmax_t, 64);
  CHECK_WIDTH_ORDER(unsigned long long, uintmax_t);
  CHECK_WIDTH_ORDER(uint_fast8_t, uintmax_t);
  CHECK_WIDTH_ORDER(uint_fast16_t, uintmax_t);
  CHECK_WIDTH_ORDER(uint_fast32_t, uintmax_t);
  CHECK_WIDTH_ORDER(uint_fast64_t, uintmax_t);
  CHECK_UNSIGNED_LIMITS(uintmax_t, UINTMAX_MAX);
}

void
test_misc_limits (void)
{
  CHECK_SIGNED_LIMITS_2(__PTRDIFF_TYPE__, PTRDIFF_MIN, PTRDIFF_MAX, -65535L, 65535L);
#ifndef SIGNAL_SUPPRESS
  CHECK_LIMITS_2(sig_atomic_t, SIG_ATOMIC_MIN, SIG_ATOMIC_MAX, -127, 127, 255);
#endif
  CHECK_UNSIGNED_LIMITS_2(__SIZE_TYPE__, SIZE_MAX, 65535U);
  CHECK_LIMITS_2(__WCHAR_TYPE__, WCHAR_MIN, WCHAR_MAX, -127, 127, 255);
  CHECK_LIMITS_2(__WINT_TYPE__, WINT_MIN, WINT_MAX, -32767, 32767, 65535);
}

void
test_constants (void)
{
  CHECK_CONSTS(int_least8_t, INT8_C);
  CHECK_CONSTS(int_least16_t, INT16_C);
  CHECK_CONSTS(int_least32_t, INT32_C);
  CHECK_CONSTS(int_least64_t, INT64_C);
  CHECK_CONSTS(intmax_t, INTMAX_C);
  CHECK_CONSTS(uint_least8_t, UINT8_C);
  CHECK_CONSTS(uint_least16_t, UINT16_C);
  CHECK_CONSTS(uint_least32_t, UINT32_C);
  CHECK_CONSTS(uint_least64_t, UINT64_C);
  CHECK_CONSTS(uintmax_t, UINTMAX_C);
#if INT8_C(12) != 12
#error "INT8_C not usable in #if"
#endif
#if INT16_C(12) != 12
#error "INT16_C not usable in #if"
#endif
#if INT32_C(12) != 12
#error "INT32_C not usable in #if"
#endif
#if INT64_C(12) != 12
#error "INT64_C not usable in #if"
#endif
#if INTMAX_C(12) != 12
#error "INTMAX_C not usable in #if"
#endif
#if UINT8_C(12) != 12
#error "UINT8_C not usable in #if"
#endif
#if UINT16_C(12) != 12
#error "UINT16_C not usable in #if"
#endif
#if UINT32_C(12) != 12
#error "UINT32_C not usable in #if"
#endif
#if UINT64_C(12) != 12
#error "UINT64_C not usable in #if"
#endif
#if UINTMAX_C(12) != 12
#error "UINTMAX_C not usable in #if"
#endif
}
