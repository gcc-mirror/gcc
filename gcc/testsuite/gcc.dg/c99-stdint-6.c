/* Verify that GCC's internal notions of types in <stdint.h> agree
   with any system <inttypes.h> header.  */
/* { dg-do compile { target inttypes_types } } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */
/* { dg-options "-std=gnu99 -pedantic-errors -DNO_FAST_TYPES" { target *-*-solaris2.[789]* } } */

#include <inttypes.h>
#include <signal.h>

#define CHECK_TYPES(TYPE1, TYPE2) \
  do { TYPE1 a; TYPE2 *b = &a; TYPE2 c; TYPE1 *d = &c; } while (0)

void
check_types (void)
{
#ifdef __INT8_TYPE__
  CHECK_TYPES(__INT8_TYPE__, int8_t);
#endif
#ifdef __INT16_TYPE__
  CHECK_TYPES(__INT16_TYPE__, int16_t);
#endif
#ifdef __INT32_TYPE__
  CHECK_TYPES(__INT32_TYPE__, int32_t);
#endif
#ifdef __INT64_TYPE__
  CHECK_TYPES(__INT64_TYPE__, int64_t);
#endif
#ifdef __UINT8_TYPE__
  CHECK_TYPES(__UINT8_TYPE__, uint8_t);
#endif
#ifdef __UINT16_TYPE__
  CHECK_TYPES(__UINT16_TYPE__, uint16_t);
#endif
#ifdef __UINT32_TYPE__
  CHECK_TYPES(__UINT32_TYPE__, uint32_t);
#endif
#ifdef __UINT64_TYPE__
  CHECK_TYPES(__UINT64_TYPE__, uint64_t);
#endif
  CHECK_TYPES(__INT_LEAST8_TYPE__, int_least8_t);
  CHECK_TYPES(__INT_LEAST16_TYPE__, int_least16_t);
  CHECK_TYPES(__INT_LEAST32_TYPE__, int_least32_t);
  CHECK_TYPES(__INT_LEAST64_TYPE__, int_least64_t);
  CHECK_TYPES(__UINT_LEAST8_TYPE__, uint_least8_t);
  CHECK_TYPES(__UINT_LEAST16_TYPE__, uint_least16_t);
  CHECK_TYPES(__UINT_LEAST32_TYPE__, uint_least32_t);
  CHECK_TYPES(__UINT_LEAST64_TYPE__, uint_least64_t);
#ifndef NO_FAST_TYPES
  CHECK_TYPES(__INT_FAST8_TYPE__, int_fast8_t);
  CHECK_TYPES(__INT_FAST16_TYPE__, int_fast16_t);
  CHECK_TYPES(__INT_FAST32_TYPE__, int_fast32_t);
  CHECK_TYPES(__INT_FAST64_TYPE__, int_fast64_t);
  CHECK_TYPES(__UINT_FAST8_TYPE__, uint_fast8_t);
  CHECK_TYPES(__UINT_FAST16_TYPE__, uint_fast16_t);
  CHECK_TYPES(__UINT_FAST32_TYPE__, uint_fast32_t);
  CHECK_TYPES(__UINT_FAST64_TYPE__, uint_fast64_t);
#endif
#ifdef __INTPTR_TYPE__
  CHECK_TYPES(__INTPTR_TYPE__, intptr_t);
#endif
#ifdef __UINTPTR_TYPE__
  CHECK_TYPES(__UINTPTR_TYPE__, uintptr_t);
#endif
  CHECK_TYPES(__INTMAX_TYPE__, intmax_t);
  CHECK_TYPES(__UINTMAX_TYPE__, uintmax_t);
  CHECK_TYPES(__SIG_ATOMIC_TYPE__, sig_atomic_t);
}
