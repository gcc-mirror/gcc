/* { dg-options "-std=c++11" } */
/* Missing <cstdint>.  */

char c = INT8_MAX; // { dg-error "'INT8_MAX' was not declared" }
// { dg-message "'INT8_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }

short s = INT16_MAX; // { dg-error "'INT16_MAX' was not declared" }
// { dg-message "'INT16_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }

int i = INT32_MAX; // { dg-error "'INT32_MAX' was not declared" }
// { dg-message "'INT32_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }

long l = INT64_MAX; // { dg-error "'INT64_MAX' was not declared" }
// { dg-message "'INT64_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }

intptr_t test_intptr (void) // { dg-error "'intptr_t' does not name a type" }
// { dg-message "'intptr_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
{
  return 0;
}

int test_intptr_max (void)
{
  return (int) INTPTR_MAX; // { dg-error "'INTPTR_MAX' was not declared" }
// { dg-message "'INTPTR_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
}

uintptr_t test_uintptr (void) // { dg-error "'uintptr_t' does not name a type" }
// { dg-message "'uintptr_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
{
  return 0;
}

unsigned int test_uintptr_max (void)
{
  return (unsigned int) UINTPTR_MAX; // { dg-error "'UINTPTR_MAX' was not declared" }
// { dg-message "'UINTPTR_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
}

int8_t i8; // { dg-error "'int8_t' does not name a type" }
// { dg-message "'int8_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
int16_t i16; // { dg-error "'int16_t' does not name a type" }
// { dg-message "'int16_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
int32_t i32; // { dg-error "'int32_t' does not name a type" }
// { dg-message "'int32_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
int64_t i64; // { dg-error "'int64_t' does not name a type" }
// { dg-message "'int64_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }

void test_uint_t (void)
{
  char bu8[(unsigned int)UINT8_MAX]; // { dg-error "'UINT8_MAX' was not declared" }
  // { dg-message "'UINT8_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
  char bu16[(unsigned int)UINT16_MAX]; // { dg-error "'UINT16_MAX' was not declared" }
  // { dg-message "'UINT16_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
  char bu32[(unsigned int)UINT32_MAX]; // { dg-error "'UINT32_MAX' was not declared" }
  // { dg-message "'UINT32_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
  char bu64[(unsigned int)UINT64_MAX]; // { dg-error "'UINT64_MAX' was not declared" }
  // { dg-message "'UINT64_MAX' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }

  auto ui8 = (uint8_t) 8; // { dg-error "'uint8_t' was not declared" }
  // { dg-message "'uint8_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
  auto ui16 = (uint16_t) 16; // { dg-error "'uint16_t' was not declared" }
  // { dg-message "'uint16_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
  auto ui32 = (uint32_t) 32; // { dg-error "'uint32_t' was not declared" }
  // { dg-message "'uint32_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
  auto ui64 = (uint64_t) 64; // { dg-error "'uint64_t' was not declared" }
  // { dg-message "'uint64_t' is defined in header '<cstdint>'; this is probably fixable by adding '#include <cstdint>'" "" { target *-*-* } .-1 }
}
