/* Verify that pairs of types in <stdint.h> are corresponding types
   (requires no pointer sign warnings, so separate from
   c99-stdint-1.c).  */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -fhosted" } */

#include <stdint.h>

#define CHECK_CORRESPONDING(TYPE1, TYPE2) \
  do { TYPE1 a; TYPE2 *b = &a; TYPE2 c; TYPE1 *d = &c; } while (0)

void
check_corresponding (void)
{
#if defined(INT8_MAX) && defined(UINT8_MAX)
  CHECK_CORRESPONDING(int8_t, uint8_t);
#endif
#if defined(INT16_MAX) && defined(UINT16_MAX)
  CHECK_CORRESPONDING(int16_t, uint16_t);
#endif
#if defined(INT32_MAX) && defined(UINT32_MAX)
  CHECK_CORRESPONDING(int32_t, uint32_t);
#endif
#if defined(INT64_MAX) && defined(UINT64_MAX)
  CHECK_CORRESPONDING(int64_t, uint64_t);
#endif
  CHECK_CORRESPONDING(int_least8_t, uint_least8_t);
  CHECK_CORRESPONDING(int_least16_t, uint_least16_t);
  CHECK_CORRESPONDING(int_least32_t, uint_least32_t);
  CHECK_CORRESPONDING(int_least64_t, uint_least64_t);
  CHECK_CORRESPONDING(int_fast8_t, uint_fast8_t);
  CHECK_CORRESPONDING(int_fast16_t, uint_fast16_t);
  CHECK_CORRESPONDING(int_fast32_t, uint_fast32_t);
  CHECK_CORRESPONDING(int_fast64_t, uint_fast64_t);
#if defined(INTPTR_MAX) && defined(UINTPTR_MAX)
  CHECK_CORRESPONDING(intptr_t, uintptr_t);
#endif
  CHECK_CORRESPONDING(intmax_t, uintmax_t);
}
