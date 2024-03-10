/* { dg-options "-std=gnu99" } */
/* Missing <stdint.h>.  */

char c = INT8_MAX; // { dg-error "'INT8_MAX' undeclared" }
// { dg-message "'INT8_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }

short s = INT16_MAX; // { dg-error "'INT16_MAX' undeclared" }
// { dg-message "'INT16_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }

int i = INT32_MAX; // { dg-error "'INT32_MAX' undeclared" }
// { dg-message "'INT32_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }

long l = INT64_MAX; // { dg-error "'INT64_MAX' undeclared" }
// { dg-message "'INT64_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }

intptr_t test_intptr (void) // { dg-error "unknown type name 'intptr_t'" }
// { dg-message "'intptr_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
{
  return INTPTR_MAX; // { dg-error "'INTPTR_MAX' undeclared" }
// { dg-message "'INTPTR_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
}

uintptr_t test_uintptr (void) // { dg-error "unknown type name 'uintptr_t'" }
// { dg-message "'uintptr_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
{
  return UINTPTR_MAX; // { dg-error "'UINTPTR_MAX' undeclared" }
// { dg-message "'UINTPTR_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
}

int8_t i8; // { dg-error "unknown type name 'int8_t'" }
// { dg-message "'int8_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
int16_t i16; // { dg-error "unknown type name 'int16_t'" }
// { dg-message "'int16_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
int32_t i32; // { dg-error "unknown type name 'int32_t'" }
// { dg-message "'int32_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
int64_t i64; // { dg-error "unknown type name 'int64_t'" }
// { dg-message "'int64_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }

void test_uint_t (void)
{
  char bu8[(unsigned int)UINT8_MAX]; // { dg-error "'UINT8_MAX' undeclared" }
  // { dg-message "'UINT8_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
  char bu16[(unsigned int)UINT16_MAX]; // { dg-error "'UINT16_MAX' undeclared" }
  // { dg-message "'UINT16_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
  char bu32[(unsigned int)UINT32_MAX]; // { dg-error "'UINT32_MAX' undeclared" }
  // { dg-message "'UINT32_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }
  char bu64[(unsigned int)UINT64_MAX]; // { dg-error "'UINT64_MAX' undeclared" }
  // { dg-message "'UINT64_MAX' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-1 }

  char ui8 = (uint8_t) 8; // { dg-error "'uint8_t' undeclared" }
  // { dg-error "expected" "" { target *-*-* } .-1 }
  // { dg-message "'uint8_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-2 }
  short ui16 = (uint16_t) 16; // { dg-error "'uint16_t' undeclared" }
  // { dg-error "expected" "" { target *-*-* } .-1 }
  // { dg-message "'uint16_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-2 }
  int ui32 = (uint32_t) 32; // { dg-error "'uint32_t' undeclared" }
  // { dg-error "expected" "" { target *-*-* } .-1 }
  // { dg-message "'uint32_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-2 }
  long ui64 = (uint64_t) 64; // { dg-error "'uint64_t' undeclared" }
  // { dg-error "expected" "" { target *-*-* } .-1 }
  // { dg-message "'uint64_t' is defined in header '<stdint.h>'; this is probably fixable by adding '#include <stdint.h>'" "" { target *-*-* } .-2 }
}
