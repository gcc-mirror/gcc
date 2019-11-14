/* { dg-do compile } */

#include <arm_sve.h>
#include <stdbool.h>

enum signed_enum { SA = -1, SB };
enum unsigned_enum { UA, UB };

void
test (int8_t s8, int16_t s16, int32_t s32, int64_t s64,
      uint8_t u8, uint16_t u16, uint32_t u32, uint64_t u64,
      bool b, enum signed_enum se, enum unsigned_enum ue,
      int *ptr, float f32, svbool_t pg, svint32_t vec)
{
  svwhilele_b8 (s32); /* { dg-error {too few arguments to function 'svwhilele_b8'} } */
  svwhilele_b8 (s32, s32, s32); /* { dg-error {too many arguments to function 'svwhilele_b8'} } */

  svwhilele_b8 (b, b);
  svwhilele_b8 (se, se);
  svwhilele_b8 (ue, ue);
  svwhilele_b8 (s8, s8);
  svwhilele_b8 (u8, u8);
  svwhilele_b8 (s16, s16);
  svwhilele_b8 (u16, u16);
  svwhilele_b8 (ptr, ptr); /* { dg-error {passing 'int \*' to argument 1 of 'svwhilele_b8', which expects a 32-bit or 64-bit integer type} } */
  svwhilele_b8 (f32, f32); /* { dg-error {passing 'float' to argument 1 of 'svwhilele_b8', which expects a 32-bit or 64-bit integer type} } */
  svwhilele_b8 (pg, pg); /* { dg-error {passing 'svbool_t' to argument 1 of 'svwhilele_b8', which expects a 32-bit or 64-bit integer type} } */
  svwhilele_b8 (vec, vec); /* { dg-error {passing 'svint32_t' to argument 1 of 'svwhilele_b8', which expects a 32-bit or 64-bit integer type} } */

  svwhilele_b8 (s32, b);
  svwhilele_b8 (s32, se);
  svwhilele_b8 (s32, ue); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int32_t' but argument 2 has type 'uint32_t'} } */
  svwhilele_b8 (s32, s8);
  svwhilele_b8 (s32, u8);
  svwhilele_b8 (s32, s16);
  svwhilele_b8 (s32, u16);

  svwhilele_b8 (u32, b); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (u32, se); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (u32, ue);
  svwhilele_b8 (u32, s8); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (u32, u8); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (u32, s16); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (u32, u16); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */

  svwhilele_b8 (s32, s32);
  svwhilele_b8 (s32, u32); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int32_t' but argument 2 has type 'uint32_t'} } */
  svwhilele_b8 (s32, s64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int32_t' but argument 2 has type 'int64_t'} } */
  svwhilele_b8 (s32, u64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int32_t' but argument 2 has type 'uint64_t'} } */

  svwhilele_b8 (u32, s32); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (u32, u32);
  svwhilele_b8 (u32, s64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int64_t'} } */
  svwhilele_b8 (u32, u64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'uint64_t'} } */

  svwhilele_b8 (s64, s32); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int64_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (s64, u32); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int64_t' but argument 2 has type 'uint32_t'} } */
  svwhilele_b8 (s64, s64);
  svwhilele_b8 (s64, u64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int64_t' but argument 2 has type 'uint64_t'} } */

  svwhilele_b8 (u64, s32); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint64_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (u64, u32); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint64_t' but argument 2 has type 'uint32_t'} } */
  svwhilele_b8 (u64, s64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint64_t' but argument 2 has type 'int64_t'} } */
  svwhilele_b8 (u64, u64);

  svwhilele_b8 (0, s32);
  svwhilele_b8 (0, u32); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int32_t' but argument 2 has type 'uint32_t'} } */
  svwhilele_b8 (0, s64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int32_t' but argument 2 has type 'int64_t'} } */
  svwhilele_b8 (0, u64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int32_t' but argument 2 has type 'uint64_t'} } */

  svwhilele_b8 (s32, 0);
  svwhilele_b8 (u32, 0); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (s64, 0); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int64_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (u64, 0); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint64_t' but argument 2 has type 'int32_t'} } */

  svwhilele_b8 (0U, s32); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int32_t'} } */
  svwhilele_b8 (0U, u32);
  svwhilele_b8 (0U, s64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'int64_t'} } */
  svwhilele_b8 (0U, u64); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint32_t' but argument 2 has type 'uint64_t'} } */

  svwhilele_b8 (s32, 0U); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int32_t' but argument 2 has type 'uint32_t'} } */
  svwhilele_b8 (u32, 0U);
  svwhilele_b8 (s64, 0U); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'int64_t' but argument 2 has type 'uint32_t'} } */
  svwhilele_b8 (u64, 0U); /* { dg-error {call to 'svwhilele_b8' is ambiguous; argument 1 has type 'uint64_t' but argument 2 has type 'uint32_t'} } */
}
