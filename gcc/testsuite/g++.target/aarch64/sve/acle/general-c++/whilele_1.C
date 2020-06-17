// { dg-do compile { target { ! ilp32 } } }

#include <arm_sve.h>

enum foo { A, B };

void
test (int8_t s8, int16_t s16, int32_t s32, int64_t s64,
      uint8_t u8, uint16_t u16, uint32_t u32, uint64_t u64,
      bool b, foo e, int *ptr, float f32, svbool_t pg,
      svint32_t vec)
{
  svwhilele_b8 (s32); // { dg-error {no matching function for call to 'svwhilele_b8\(int32_t&\)'} }
  svwhilele_b8 (s32, s32, s32); // { dg-error {no matching function for call to 'svwhilele_b8\(int32_t&, int32_t&, int32_t&\)'} }

  svwhilele_b8 (b, b);
  svwhilele_b8 (e, e);
  svwhilele_b8 (s8, s8);
  svwhilele_b8 (u8, u8);
  svwhilele_b8 (s16, s16);
  svwhilele_b8 (u16, u16);
  svwhilele_b8 (ptr, ptr); // { dg-error {no matching function for call to 'svwhilele_b8\(int\*&, int\*&\)'} }
  // { dg-error {invalid conversion from 'int\*' to '[^']*'} "" { target *-*-* } .-1 }
  svwhilele_b8 (f32, f32); // { dg-error {call of overloaded 'svwhilele_b8\(float&, float&\)' is ambiguous} }
  svwhilele_b8 (pg, pg); // { dg-error {no matching function for call to 'svwhilele_b8\(svbool_t&, svbool_t&\)'} }
  svwhilele_b8 (vec, vec); // { dg-error {no matching function for call to 'svwhilele_b8\(svint32_t&, svint32_t&\)'} }

  svwhilele_b8 (s32, b);
  svwhilele_b8 (s32, e);
  svwhilele_b8 (s32, s8);
  svwhilele_b8 (s32, u8);
  svwhilele_b8 (s32, s16);
  svwhilele_b8 (s32, u16);

  svwhilele_b8 (u32, b); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, bool&\)' is ambiguous} }
  svwhilele_b8 (u32, e); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, foo&\)' is ambiguous} }
  svwhilele_b8 (u32, s8); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, int8_t&\)' is ambiguous} }
  svwhilele_b8 (u32, u8); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, uint8_t&\)' is ambiguous} }
  svwhilele_b8 (u32, s16); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, int16_t&\)' is ambiguous} }
  svwhilele_b8 (u32, u16); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, uint16_t&\)' is ambiguous} }

  svwhilele_b8 (s32, s32);
  svwhilele_b8 (s32, u32); // { dg-error {call of overloaded 'svwhilele_b8\(int32_t&, uint32_t&\)' is ambiguous} }
  svwhilele_b8 (s32, s64); // { dg-error {call of overloaded 'svwhilele_b8\(int32_t&, int64_t&\)' is ambiguous} }
  svwhilele_b8 (s32, u64); // { dg-error {call of overloaded 'svwhilele_b8\(int32_t&, uint64_t&\)' is ambiguous} }

  svwhilele_b8 (u32, s32); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, int32_t&\)' is ambiguous} }
  svwhilele_b8 (u32, u32);
  svwhilele_b8 (u32, s64); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, int64_t&\)' is ambiguous} }
  svwhilele_b8 (u32, u64); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, uint64_t&\)' is ambiguous} }

  svwhilele_b8 (s64, s32); // { dg-error {call of overloaded 'svwhilele_b8\(int64_t&, int32_t&\)' is ambiguous} }
  svwhilele_b8 (s64, u32); // { dg-error {call of overloaded 'svwhilele_b8\(int64_t&, uint32_t&\)' is ambiguous} }
  svwhilele_b8 (s64, s64);
  svwhilele_b8 (s64, u64); // { dg-error {call of overloaded 'svwhilele_b8\(int64_t&, uint64_t&\)' is ambiguous} }

  svwhilele_b8 (u64, s32); // { dg-error {call of overloaded 'svwhilele_b8\(uint64_t&, int32_t&\)' is ambiguous} }
  svwhilele_b8 (u64, u32); // { dg-error {call of overloaded 'svwhilele_b8\(uint64_t&, uint32_t&\)' is ambiguous} }
  svwhilele_b8 (u64, s64); // { dg-error {call of overloaded 'svwhilele_b8\(uint64_t&, int64_t&\)' is ambiguous} }
  svwhilele_b8 (u64, u64);

  svwhilele_b8 (0, s32);
  svwhilele_b8 (0, u32); // { dg-error {call of overloaded 'svwhilele_b8\(int, uint32_t&\)' is ambiguous} }
  svwhilele_b8 (0, s64); // { dg-error {call of overloaded 'svwhilele_b8\(int, int64_t&\)' is ambiguous} }
  svwhilele_b8 (0, u64); // { dg-error {call of overloaded 'svwhilele_b8\(int, uint64_t&\)' is ambiguous} }

  svwhilele_b8 (s32, 0);
  svwhilele_b8 (u32, 0); // { dg-error {call of overloaded 'svwhilele_b8\(uint32_t&, int\)' is ambiguous} }
  svwhilele_b8 (s64, 0); // { dg-error {call of overloaded 'svwhilele_b8\(int64_t&, int\)' is ambiguous} }
  svwhilele_b8 (u64, 0); // { dg-error {call of overloaded 'svwhilele_b8\(uint64_t&, int\)' is ambiguous} }

  svwhilele_b8 (0U, s32); // { dg-error {call of overloaded 'svwhilele_b8\(unsigned int, int32_t&\)' is ambiguous} }
  svwhilele_b8 (0U, u32);
  svwhilele_b8 (0U, s64); // { dg-error {call of overloaded 'svwhilele_b8\(unsigned int, int64_t&\)' is ambiguous} }
  svwhilele_b8 (0U, u64); // { dg-error {call of overloaded 'svwhilele_b8\(unsigned int, uint64_t&\)' is ambiguous} }

  svwhilele_b8 (s32, 0U); // { dg-error {call of overloaded 'svwhilele_b8\(int32_t&, unsigned int\)' is ambiguous} }
  svwhilele_b8 (u32, 0U);
  svwhilele_b8 (s64, 0U); // { dg-error {call of overloaded 'svwhilele_b8\(int64_t&, unsigned int\)' is ambiguous} }
  svwhilele_b8 (u64, 0U); // { dg-error {call of overloaded 'svwhilele_b8\(uint64_t&, unsigned int\)' is ambiguous} }
}
